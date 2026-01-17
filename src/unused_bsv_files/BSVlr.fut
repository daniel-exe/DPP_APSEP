-- Berkman & Vishkin ANSV skeleton in Futhark

import "reduction_tree_test"  -- or wherever your binary reduction tree modules are

let findRepresentative [n] (arr: [n]i64) : i64 =
  reduce (\i1 i2 -> if arr[i2] < arr[i1] then i2 else i1) 0 (iota n)

-- Find left and right match of a representative using the tree
let findLeftRightMatch (tree: mintree.tree) (i: i64) : (i64, i64) =
  let l = mintree.previous tree i
  let r = mintree.next tree i
  in (l, r)
  
-- Far-away merging (linear merge of blocks)
let farAwayBlocks_ANSV_linear [n] (L: [n]i64) (R: [n]i64) (LR: [n]i64) (RL: [n]i64): ([n]i64, [n]i64) =
  let (Lr_final, Rl_final, _, _) =
    loop (Lr, Rl, i, j) = (copy LR, copy RL, n - 1, 0)
      while i <= j do
        let (Lr_next, Rl_next, i_next, j_next) =
          if L[i] < R[j] then
            let Lr_new = if Lr[j] == -1 then Lr with [j] = i else Lr
            in (Lr_new, Rl, i, j + 1)
          else
            let Rl_new = if Rl[i] == -1 then Rl with [i] = j else Rl
            in (Lr, Rl_new, i - 1, j)
        in (Lr_next, Rl_next, i_next, j_next)
  in (Lr_final, Rl_final)

-- Adjacent merge for a block (stack-based nearest smaller scan)
let adjacentMergeBOTH [n] (L: [n]i64) (R: [n]i64) (LR: [n]i64) (RL: [n]i64): ([n]i64, [n]i64) =

  -- Forward scan for L (previous smaller)
  let (Lr_final, _, _) =
    loop (Lr, top, stack) = (copy LR, -1i64, replicate n 0i64) 
      for i in 0 ... (n - 1) do
        let rec_top = loop top_acc = top 
                      while top_acc >= 0 && L[stack[top_acc]] > L[i] do top_acc - 1
        let Lr2 = if rec_top >= 0 then Lr with [i] = stack[rec_top] else Lr
        let top2 = rec_top + 1
        let stack2 = stack with [top2] = i
        in (Lr2, top2, stack2)
  in

  -- Backward scan for R (next smaller)
  let (Rl_final, _, _) =
    loop (Rl, top, stack) = (copy RL, -1i64, replicate n 0i64) 
      for i in (n - 1) .. (n - 2)... 0 do
        let rec_top = loop top_acc = top 
                      while top_acc >= 0 && R[stack[top_acc]] > R[i] do top_acc - 1
        let Rl2 = if rec_top >= 0 then Rl with [i] = stack[rec_top] else Rl
        let top2 = rec_top + 1
        let stack2 = stack with [top2] = i
        in (Rl2, top2, stack2)
  in

  (Lr_final, Rl_final)


-- entries

entry findRepresentative_entry [n] (A: [n]i64) : i64 =
  findRepresentative A

entry adjacentMerge_entry [n] (L: [n]i64) (R: [n]i64) (LR: [n]i64) (RL: [n]i64) : ([]i64, []i64) =
  adjacentMergeBOTH L R LR RL

entry farAwayBlocks_entry [n] (L: [n]i64) (R: [n]i64) (LR: [n]i64) (RL: [n]i64) : ([]i64, []i64) =
  farAwayBlocks_ANSV_linear L R LR RL

entry findLeftRightMatch_entry (A: []i64) (i: i64) : [2]i64 =
  let tree = mintree.make A
  let (l,r) = findLeftRightMatch tree i
  in [l,r]


-- ==
-- entry: findRepresentative_entry
-- input {[5i64,2i64,7i64,2i64,9i64]}
-- output {1i64}
-- input {[3i64,1i64,4i64,0i64]}
-- output {3i64}

-- ==
-- entry: adjacentMerge_entry
-- input {[5i64,2i64,1i64,4i64] [5i64,2i64,1i64,4i64] [-1i64,-1i64,-1i64,-1i64] [-1i64,-1i64,-1i64,-1i64]}
-- output {[-1i64,-1i64,-1i64,2i64] [-1i64,2i64,-1i64,-1i64]}
-- input {[1i64,2i64,3i64,4i64] [1i64,2i64,3i64,4i64] [-1i64,-1i64,-1i64,-1i64] [-1i64,-1i64,-1i64,-1i64]}
-- output {[-1i64,0i64,1i64,2i64] [-1i64,-1i64,-1i64,-1i64]}
-- input {[4i64,3i64,2i64,1i64] [4i64,3i64,2i64,1i64] [-1i64,-1i64,-1i64,-1i64] [-1i64,-1i64,-1i64,-1i64]}
-- output {[-1i64,-1i64,-1i64,-1i64] [1i64,2i64,3i64,-1i64]}

-- ==
-- entry: findLeftRightMatch_entry
-- input {[5i64,2i64,6i64,1i64,4i64] 2i64}
-- output {[1i64,3i64]}
-- input {[8i64,6i64,7i64,5i64,3i64,4i64] 2i64}
-- output {[1i64,3i64]}

-- ==
-- entry: farAwayBlocks_entry
-- input {[8i64,6i64,7i64,5i64,3i64,4i64] [5i64,2i64,6i64,1i64,3i64,4i64] [-1i64,-1i64,-1i64,-1i64,-1i64,-1i64] [-1i64,-1i64,-1i64,-1i64,-1i64,-1i64]}
-- output {[-1i64,-1i64,-1i64,0i64,1i64,2i64] [3i64,3i64,3i64,-1i64,-1i64,-1i64]}



