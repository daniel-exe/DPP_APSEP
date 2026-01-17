-- Benchmark harness: compare 2 BSZ and 1 BSV implementations vs baselines.
--
-- BSZ implementations assume k divides n.

module bsz = import "bsz"
module bsz_fast = import "bsz_fast"
module bsv = import "bsv"

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

-- bsz.fut: local matches via naive scan inside each block.
-- ==
-- entry: bench_bsz
-- compiled random input { [1048576]i32 256i64 }
-- compiled random input { [4194304]i32 512i64 }
entry bench_bsz [n] (A: [n]i32) (k: i64) : [n]i64 =
  bsz.BSZ A k

-- bsz_fast.fut: local matches via per-block mintree queries using map.
-- ==
-- entry: bench_bsz_fast
-- compiled random input { [1048576]i32 256i64 }
-- compiled random input { [4194304]i32 512i64 }
entry bench_bsz_fast [n] (A: [n]i32) (k: i64) : [n]i64 =
  bsz_fast.BSZ A k

-- BSV implementation

-- bsv.fut:
-- ==
-- entry: bench_bsv
-- compiled random input { [1048576]i64 256i64 }
-- compiled random input { [4194304]i64 512i64 }
entry bench_bsv [n] (A: [n]i64) (k: i64) : ([n]i64, [n]i64) =
  bsv.ANSV_Berkman A k
