-- Benchmark harness: compare BSZ and BSV implementations vs baselines.
--
-- BSZ implementations assume k divides n.

module bsz = import "bsz"
module bsv = import "bsv"

module rt  = import "reduction_tree/reduction_tree"
module rtt = import "reduction_tree/reduction_tree_test"

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
-- compiled random input { [1048576]i64 }
-- compiled random input { [4194304]i64 }
entry bench_mintree_strict_previous [n] (A: [n]i64) : [n]i64 =
  let t = mintree.make A
  in tabulate n (mintree.strict_previous t)

-- O(n^2): naive backwards linear search for every i.
-- ==
-- entry: bench_linear_strict_previous
-- compiled random input { [1048576]i64 }
-- compiled random input { [4194304]i64 }
entry bench_linear_strict_previous [n] (A: [n]i64) : [n]i64 =
  tabulate n (rtt.backwards_linear_search (<) A)

-- bsz.fut: local matches via naive scan inside each block.
-- ==
-- entry: bench_bsz
-- compiled random input { [1048576]i64 256i64 }
-- compiled random input { [1048576]i64 512i64 }
-- compiled random input { [1048576]i64 1024i64 }
-- compiled random input { [1048576]i64 2048i64 }
-- compiled random input { [1048576]i64 4096i64 }
-- compiled random input { [4194304]i64 256i64 }
-- compiled random input { [4194304]i64 512i64 }
-- compiled random input { [4194304]i64 1024i64 }
-- compiled random input { [4194304]i64 2048i64 }
-- compiled random input { [4194304]i64 4096i64 }
entry bench_bsz [n] (A: [n]i64) (k: i64) : [n]i64 =
  bsz.BSZ A k

-- bsv.fut:
-- ==
-- entry: bench_bsv
-- compiled random input { [1048576]i64 256i64 }
-- compiled random input { [1048576]i64 512i64 }
-- compiled random input { [1048576]i64 1024i64 }
-- compiled random input { [1048576]i64 2048i64 }
-- compiled random input { [1048576]i64 4096i64 }
-- compiled random input { [4194304]i64 256i64 }
-- compiled random input { [4194304]i64 512i64 }
-- compiled random input { [4194304]i64 1024i64 }
-- compiled random input { [4194304]i64 2048i64 }
-- compiled random input { [4194304]i64 4096i64 }
entry bench_bsv [n] (A: [n]i64) (k: i64) : ([n]i64, [n]i64) =
  bsv.ANSV_Berkman A k
