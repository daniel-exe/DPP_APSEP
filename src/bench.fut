-- Benchmark harness: compare BSZ and BSV implementations vs baselines.
--
-- BSZ implementations assume k divides n.

module bsz = import "bsz"
module bsv = import "bsv"

module rt  = import "lib/reduction_tree/reduction_tree"
module rtt = import "lib/reduction_tree/reduction_tree_test"

module shuffle = import "lib/cpprandom/shuffle"
module shfl = shuffle.mk_shuffle u32 shuffle.pcg32

let mk_rng (seed: i64) (n: i64) =
  shuffle.pcg32.rng_from_seed
    [ i32.u32 (u32.i64 seed)
    , i32.u32 (u32.i64 n)
    , 42i32
    ]

let gen_shuffled_iota (n: i64) (seed: i64) : [n]i64 =
  let xs: [n]i64 = iota n
  let rng = mk_rng seed n
  let (_, ys) = shfl.shuffle rng xs
  in ys

module mintree = rt.mk_mintree {
  type t = i64
  let highest = i64.highest
  let min (x: i64) (y: i64) : i64 = if x < y then x else y
  let (<=) (x: i64) (y: i64) : bool = x <= y
  let (<)  (x: i64) (y: i64) : bool = x < y
}

-- Baselines

-- O(n log n): build reduction tree once, query strict_previous for every i.
-- ==
-- entry: bench_mintree_strict_previous
-- compiled input { 65536i64 }
entry bench_mintree_strict_previous (n: i64) : [n]i64 =
  let A = gen_shuffled_iota n 1
  let t = mintree.make A
  in tabulate n (mintree.strict_previous t)

-- bsz.fut: local matches via naive scan inside each block.
-- ==
-- entry: bench_bsz
-- compiled input { 65536i64 64i64 }
-- compiled input { 65536i64 128i64 }
-- compiled input { 65536i64 256i64 }
-- compiled input { 65536i64 512i64 }
-- compiled input { 65536i64 1024i64 }
-- compiled input { 65536i64 2048i64 }
-- compiled input { 65536i64 4096i64 }
-- compiled input { 65536i64 8192i64 }
-- compiled input { 65536i64 16384i64 }
-- compiled input { 65536i64 32768i64 }
-- compiled input { 65536i64 65536i64 }
entry bench_bsz (n: i64) (k: i64) : [n]i64 =
  let A = gen_shuffled_iota n 1
  in bsz.BSZ A k

-- bsv.fut:
-- ==
-- entry: bench_bsv
-- compiled input { 65536i64 1i64 }
-- compiled input { 65536i64 2i64 }
-- compiled input { 65536i64 4i64 }
-- compiled input { 65536i64 16i64 }
-- compiled input { 65536i64 64i64 }
-- compiled input { 65536i64 128i64 }
-- compiled input { 65536i64 256i64 }
-- compiled input { 65536i64 512i64 }
-- compiled input { 65536i64 1024i64 }
entry bench_bsv (n: i64) (k: i64) : ([n]i64, [n]i64) =
  let A = gen_shuffled_iota n 1
  in bsv.ANSV_Berkman A k
