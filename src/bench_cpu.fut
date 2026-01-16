-- Benchmark harness: compare 3 BSZ implementations vs baselines.
--
-- BSZ implementations assume k divides n.

module bsz_seq = import "bsz_seq"
module bsz_cpu = import "bsz_mintree_cpu"
module bsz_gpu = import "bsz_mintree_gpu"

module rt  = import "reduction_tree"
module rtt = import "reduction_tree_test"

module mintree = rt.mk_mintree {
  type t = i32
  let highest = 2147483647i32
  let min (x: i32) (y: i32) : i32 = if x < y then x else y
  let (<=) (x: i32) (y: i32) : bool = x <= y
  let (<)  (x: i32) (y: i32) : bool = x < y
}

-- Baselines

-- O(n log n): build reduction tree once, query strict_previous for every i.
-- ==
-- entry: bench_mintree_strict_previous
-- compiled random input { [1048576]i32 }
-- compiled random input { [4194304]i32 }
entry bench_mintree_strict_previous [n] (A: [n]i32) : [n]i64 =
  let t = mintree.make A
  in tabulate n (mintree.strict_previous t)

-- O(n^2): naive backwards linear search for every i.
-- ==
-- entry: bench_linear_strict_previous
-- compiled random input { [1048576]i32 }
-- compiled random input { [4194304]i32 }
entry bench_linear_strict_previous [n] (A: [n]i32) : [n]i64 =
  tabulate n (rtt.backwards_linear_search (<) A)

-- BSZ implementations

-- bsz_seq.fut: local matches via naive scan inside each block.
-- ==
-- entry: bench_bsz_seq
-- compiled random input { [1048576]i32 256i64 }
-- compiled random input { [4194304]i32 512i64 }
entry bench_bsz_seq [n] (A: [n]i32) (k: i64) : [n]i64 =
  bsz_seq.BSZ A k

-- bsz_mintree_cpu.fut: local matches via per-block mintree queries using map.
-- ==
-- entry: bench_bsz_mintree_cpu
-- compiled random input { [1048576]i32 256i64 }
-- compiled random input { [4194304]i32 512i64 }
entry bench_bsz_mintree_cpu [n] (A: [n]i32) (k: i64) : [n]i64 =
  bsz_cpu.BSZ A k

-- bsz_mintree_gpu.fut: local matches via sequential loop filling a unique 2D array.
-- ==
-- entry: bench_bsz_mintree_gpu
-- compiled random input { [1048576]i32 256i64 }
-- compiled random input { [4194304]i32 512i64 }
entry bench_bsz_mintree_gpu [n] (A: [n]i32) (k: i64) : [n]i64 =
  bsz_gpu.BSZ A k
