#!/usr/bin/env python3
"""Plot Futhark `futhark bench --json` results.

This script is tailored for benchmarking experiments where:
- You benchmark multiple "programs" (e.g., bench_bsz vs bench_bsv).
- You sweep a single integer parameter k (e.g., number of blocks), while n is fixed.
- You may have a baseline algorithm (e.g., a tree / mintree) that produces a *single* timing
  for the same n, which you want to compare against the sweep.

Input
- A JSON file produced by:  futhark bench --json ...
  (Your uploaded file has keys like: "bench.fut:bench_bsz", "bench.fut:bench_bsv", etc.)

Notes
- Runtimes in Futhark JSON are microseconds.
- CI here is a simple normal approximation on the mean.
  If you want bootstrap CIs, it's easy to add (but slower).
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import os
import re
import statistics as stats
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

DATASET_RE = re.compile(r"^\[(?P<n>\d+)\]i64(?:\s+(?P<k>\d+)i64)?$")

HASHED_RE = re.compile(r'^#\d+\s+\("(?P<body>[^"]+)"\)\s*$')

BODY_RE = re.compile(r'^(?P<n>\d+)i64(?:\s+(?P<k>\d+)i64)?$')



@dataclass(frozen=True)
class Point:
    n: int
    k: Optional[int]  # None for single-parameter datasets (baseline)
    runtimes_us: List[float]

    @property
    def mean_us(self) -> float:
        return float(stats.mean(self.runtimes_us))

    @property
    def stdev_us(self) -> float:
        if len(self.runtimes_us) < 2:
            return 0.0
        return float(stats.stdev(self.runtimes_us))

    @property
    def ci95_mean_us(self) -> float:
        """Half-width of 95% CI for the mean (normal approx)."""
        m = len(self.runtimes_us)
        if m < 2:
            return 0.0
        return 1.96 * self.stdev_us / math.sqrt(m)


def parse_dataset_key(key: str) -> Tuple[int, Optional[int]]:
    """Parse dataset strings from various Futhark JSON formats."""
    key = key.strip()

    # Old style: "[n]i64 k i64"
    m = DATASET_RE.match(key)
    if m:
        n = int(m.group("n"))
        k_str = m.group("k")
        return n, int(k_str) if k_str is not None else None

    mh = HASHED_RE.match(key)
    if mh:
        body = mh.group("body").strip()
        mb = BODY_RE.match(body)
        if not mb:
            raise ValueError(f"Unrecognized hashed dataset body: {body!r} from key {key!r}")
        n = int(mb.group("n"))
        k_str = mb.group("k")
        return n, int(k_str) if k_str is not None else None

    raise ValueError(f"Unrecognized dataset key format: {key!r}")




def load_futhark_bench_json(path: Path) -> Dict[str, List[Point]]:
    """Return mapping: program_name -> list of Points."""
    raw = json.loads(path.read_text())
    out: Dict[str, List[Point]] = {}

    for prog_name, prog_obj in raw.items():
        datasets = prog_obj.get("datasets", {})
        pts: List[Point] = []
        for ds_key, ds_obj in datasets.items():
            n, k = parse_dataset_key(ds_key)
            runtimes = ds_obj.get("runtimes", [])
            if not runtimes:
                continue
            pts.append(Point(n=n, k=k, runtimes_us=[float(x) for x in runtimes]))
        pts.sort(key=lambda p: (-1 if p.k is None else p.k))
        out[prog_name] = pts

    return out


def label_map_from_args(items: List[str]) -> Dict[str, str]:
    """Parse repeated --label entries in the form key=Pretty Label."""
    m: Dict[str, str] = {}
    for item in items:
        if "=" not in item:
            raise ValueError(f"--label must be KEY=LABEL, got: {item!r}")
        k, v = item.split("=", 1)
        m[k.strip()] = v.strip()
    return m


def ensure_outdir(p: Path) -> None:
    p.mkdir(parents=True, exist_ok=True)


def common_n(points_by_prog: Dict[str, List[Point]], progs: Iterable[str]) -> Optional[int]:
    ns = []
    for prog in progs:
        pts = points_by_prog.get(prog, [])
        nset = {p.n for p in pts}
        if len(nset) == 1:
            ns.append(next(iter(nset)))
        elif len(nset) > 1:
            # If you somehow benchmarked multiple n's, we won't guess.
            return None
    if not ns:
        return None
    # If they disagree, also bail.
    if len(set(ns)) == 1:
        return ns[0]
    return None


def pick_baseline_time_us(points_by_prog: Dict[str, List[Point]], baseline_prog: str, mode: str) -> Tuple[float, float]:
    """Return (baseline_mean_us, baseline_ci95_us)."""
    pts = points_by_prog.get(baseline_prog)
    if not pts:
        raise ValueError(f"Baseline program not found or has no data: {baseline_prog}")

    # If baseline has a single dataset with no k, use it.
    nok = [p for p in pts if p.k is None]
    if len(nok) == 1:
        p = nok[0]
        return p.mean_us, p.ci95_mean_us

    # Otherwise baseline is a sweep; choose by mode.
    sweep = [p for p in pts if p.k is not None]
    if not sweep:
        raise ValueError(f"Baseline program has no usable datasets: {baseline_prog}")

    if mode == "best":
        p = min(sweep, key=lambda x: x.mean_us)
        return p.mean_us, p.ci95_mean_us
    if mode == "worst":
        p = max(sweep, key=lambda x: x.mean_us)
        return p.mean_us, p.ci95_mean_us
    if mode == "k=min":
        p = min(sweep, key=lambda x: x.k)  # type: ignore[arg-type]
        return p.mean_us, p.ci95_mean_us
    if mode == "k=max":
        p = max(sweep, key=lambda x: x.k)  # type: ignore[arg-type]
        return p.mean_us, p.ci95_mean_us

    raise ValueError(f"Unknown baseline mode: {mode}")


def to_unit(us: float, unit: str) -> float:
    unit = unit.lower()
    if unit == "us":
        return us
    if unit == "ms":
        return us / 1_000.0
    if unit == "s":
        return us / 1_000_000.0
    raise ValueError(f"Unknown unit: {unit}")


def auto_label(prog_key: str) -> str:
    """Make short legend labels from Futhark program keys.

    Examples:
      bench.fut:bench_bsz -> bsz
      bench.fut:bench_bsv -> bsv
      bench.fut:bench_mintree_strict_previous -> mintree
    """
    k = prog_key.split(':')[-1]
    k = k.replace('bench_', '')
    if 'mintree' in k:
        return 'mintree'
    return k



def plot_runtime_vs_k(
    points_by_prog: Dict[str, List[Point]],
    progs: List[str],
    labels: Dict[str, str],
    baseline_prog: Optional[str],
    baseline_mode: str,
    title: str,
    outdir: Path,
    unit: str,
    logx: bool,
    logy: bool,
    errorbars: bool,
    fmt: str,
) -> None:
    ensure_outdir(outdir)

    fig = plt.figure(figsize=(8.2, 4.8))
    ax = fig.add_subplot(1, 1, 1)

    # Plot swept series.
    summary_rows: List[Dict[str, object]] = []

    for prog in progs:
        pts = [p for p in points_by_prog.get(prog, []) if p.k is not None]
        if not pts:
            continue
        ks = [p.k for p in pts]  # type: ignore[list-item]
        ys = [to_unit(p.mean_us, unit) for p in pts]
        yerr = [to_unit(p.ci95_mean_us, unit) for p in pts]
        label = labels.get(prog, auto_label(prog))

        if errorbars:
            ax.errorbar(ks, ys, yerr=yerr, marker="o", linewidth=1.5, capsize=3, label=label)
        else:
            ax.plot(ks, ys, marker="o", linewidth=1.5, label=label)

        for p in pts:
            summary_rows.append({
                "program": prog,
                "label": label,
                "n": p.n,
                "k": p.k,
                f"mean_{unit}": to_unit(p.mean_us, unit),
                f"ci95_{unit}": to_unit(p.ci95_mean_us, unit),
                "samples": len(p.runtimes_us),
            })

    # Baseline as horizontal line + band.
    if baseline_prog is not None:
        b_mean_us, b_ci_us = pick_baseline_time_us(points_by_prog, baseline_prog, baseline_mode)
        b_mean = to_unit(b_mean_us, unit)
        b_ci = to_unit(b_ci_us, unit)
        b_label = labels.get(baseline_prog, auto_label(baseline_prog))

        ax.axhline(b_mean, linestyle="--", linewidth=1.5, label=f"Baseline: {b_label}")
        if b_ci > 0:
            ax.axhspan(b_mean - b_ci, b_mean + b_ci, alpha=0.15)

    ax.set_title(title)
    ax.set_xlabel("k (blocks)")
    ax.set_ylabel(f"Runtime ({unit})")

    if logx:
        ax.set_xscale("log", base=2)
    if logy:
        ax.set_yscale("log")

    ax.grid(True, which="both", linestyle=":", linewidth=0.7)
    ax.legend()
    fig.tight_layout()

    out_path = outdir / f"runtime_vs_k.{fmt}"
    fig.savefig(out_path)
    plt.close(fig)


def plot_speedup_vs_k(
    points_by_prog: Dict[str, List[Point]],
    progs: List[str],
    labels: Dict[str, str],
    baseline_prog: str,
    baseline_mode: str,
    title: str,
    outdir: Path,
    logx: bool,
    fmt: str,
) -> None:
    ensure_outdir(outdir)

    b_mean_us, _b_ci_us = pick_baseline_time_us(points_by_prog, baseline_prog, baseline_mode)

    fig = plt.figure(figsize=(8.2, 4.8))
    ax = fig.add_subplot(1, 1, 1)

    summary_rows: List[Dict[str, object]] = []

    for prog in progs:
        pts = [p for p in points_by_prog.get(prog, []) if p.k is not None]
        if not pts:
            continue
        ks = [p.k for p in pts]  # type: ignore[list-item]
        speed = [b_mean_us / p.mean_us for p in pts]
        label = labels.get(prog, auto_label(prog))
        ax.plot(ks, speed, marker="o", linewidth=1.5, label=label)

        for p, s in zip(pts, speed):
            summary_rows.append({
                "program": prog,
                "label": label,
                "n": p.n,
                "k": p.k,
                "speedup": s,
                "baseline_program": baseline_prog,
                "baseline_mode": baseline_mode,
            })

    b_label = labels.get(baseline_prog, auto_label(baseline_prog))
    ax.axhline(1.0, linestyle=":", linewidth=1.2)

    ax.set_title(title if title else f"Speedup vs k (baseline: {b_label})")
    ax.set_xlabel("k (blocks)")
    ax.set_ylabel("Speedup (baseline / runtime)")

    if logx:
        ax.set_xscale("log", base=2)

    ax.grid(True, which="both", linestyle=":", linewidth=0.7)
    ax.legend()
    fig.tight_layout()

    out_path = outdir / f"speedup_vs_k.{fmt}"
    fig.savefig(out_path)
    plt.close(fig)



def main() -> None:
    ap = argparse.ArgumentParser(
        description="Plot runtime and speedup from Futhark `futhark bench --json` output."
    )
    ap.add_argument("json", type=Path, help="Path to Futhark bench JSON")

    ap.add_argument(
        "--program",
        action="append",
        default=[],
        help="Program key to plot (repeatable). If omitted, auto-select sweep programs.",
    )
    ap.add_argument(
        "--baseline",
        default=None,
        help="Program key to use as baseline (horizontal line + speedup baseline).",
    )
    ap.add_argument(
        "--baseline-mode",
        default="best",
        choices=["best", "worst", "k=min", "k=max"],
        help="If baseline is a sweep, how to pick its reference runtime.",
    )

    ap.add_argument(
        "--label",
        action="append",
        default=[],
        help="Optional renames for legend labels: PROGRAM_KEY=Pretty Name (repeatable).",
    )

    ap.add_argument("--title", default="", help="Title for the runtime plot")
    ap.add_argument("--title-speedup", default="", help="Title for the speedup plot")

    ap.add_argument("--outdir", type=Path, default=Path("plots"), help="Output directory")
    ap.add_argument("--fmt", choices=["pdf", "png"], default="pdf", help="Output format")

    ap.add_argument("--y-unit", choices=["us", "ms", "s"], default="ms", help="Runtime unit")
    ap.add_argument("--logx", action="store_true", help="Log2 x-axis (k)")
    ap.add_argument("--logy", action="store_true", help="Log y-axis (runtime)")
    ap.add_argument("--no-errorbars", action="store_true", help="Disable 95% CI error bars")

    args = ap.parse_args()

    points_by_prog = load_futhark_bench_json(args.json)
    labels = label_map_from_args(args.label)

    # Auto-select programs if not provided: choose those that have at least 2 k-values.
    progs = list(args.program)
    if not progs:
        for prog, pts in points_by_prog.items():
            ks = sorted({p.k for p in pts if p.k is not None})
            if len(ks) >= 2:
                progs.append(prog)

    if not progs:
        raise SystemExit("No sweep programs found. Provide --program explicitly.")

    # If baseline not explicitly set, try to find a single-point program.
    baseline = args.baseline
    if baseline is None:
        single_point = [
            prog
            for prog, pts in points_by_prog.items()
            if sum(1 for p in pts if p.k is None) == 1
        ]
        # Prefer something that looks like mintree.
        mintrees = [p for p in single_point if "mintree" in p]
        if len(mintrees) == 1:
            baseline = mintrees[0]
        elif len(single_point) == 1:
            baseline = single_point[0]

    # Reasonable default title.
    if not args.title:
        n = common_n(points_by_prog, progs)
        args.title = f"Runtime vs k (n={n})" if n is not None else "Runtime vs k"

    plot_runtime_vs_k(
        points_by_prog=points_by_prog,
        progs=progs,
        labels=labels,
        baseline_prog=baseline,
        baseline_mode=args.baseline_mode,
        title=args.title,
        outdir=args.outdir,
        unit=args.y_unit,
        logx=args.logx,
        logy=args.logy,
        errorbars=not args.no_errorbars,
        fmt=args.fmt,
    )

    if baseline is not None:
        plot_speedup_vs_k(
            points_by_prog=points_by_prog,
            progs=progs,
            labels=labels,
            baseline_prog=baseline,
            baseline_mode=args.baseline_mode,
            title=args.title_speedup,
            outdir=args.outdir,
            logx=args.logx,
            fmt=args.fmt,
        )

    # Print generated files (nice for Makefiles / logs).
    for name in ["runtime_vs_k", "speedup_vs_k"]:
        p = args.outdir / f"{name}.{args.fmt}"
        if p.exists():
            print(p)


if __name__ == "__main__":
    main()
