-- Berkman & Vishkin ANSV skeleton in Futhark

import "reduction_tree_test"  -- or wherever your binary reduction tree modules are

-- Compute representative (smallest index in a block)
let findRepresentative [n] (arr: [n]i64) : i64 =
  reduce (\i1 i2 -> if arr[i2] < arr[i1] then i2 else i1) 0 (iota n)

-- Far-away merging (linear merge of blocks)
let farAwayBlocks_ANSV_linear [n]
  (A: [n]i64) (a: i64) (b: i64) (c: i64) (d: i64)
  (L: [n]i64) (R: [n]i64) : ([n]i64, [n]i64) =
  if a > b || c > d then (L, R)
  else
    let (Lf, Rf, _, _) =
      loop (Left, Right, i, j) = (copy L, copy R, b, c)
        while i >= a || j <= d do
          -- if (i > a) && (j <= d) then
          if (i >= a) && (j <= d) then
            if A[i] < A[j] then
              let Left'  = if Left[j] == -1 then Left  with [j] = i else Left
              in (Left', Right, i, j + 1)
            else
              let Right' = if Right[i] == -1 then Right with [i] = j else Right
              in (Left, Right', i - 1, j)
          else if j <= d then
            -- left side exhausted (i < a): cannot assign a valid left match
            -- just advance j (leave Left[j] as-is)
            (Left, Right, i, j + 1)
          else
            -- right side exhausted (j > d): cannot assign a valid right match
            -- just advance i (leave Right[i] as-is)
            (Left, Right, i - 1, j)
    in (Lf, Rf)

let farAwayBlocks_ANSV_linear1 [n]
  (A: [n]i64) (a: i64) (b: i64) (c: i64) (d: i64)
  (L: [n]i64) (R: [n]i64) : ([n]i64, [n]i64) =
  if a == -1 || d == -1 then
    (L, R)
  else
    -- Linear scan using while loop
    let (L_final, R_final, _, _) =
      loop (Left, Right, i, j) = (copy L, copy R,b, c) 
        while i >= a || j <= d do
          let (Left_next, Right_next, i_next, j_next) =
            if A[i] < A[j] then
              let L_new = if Left[j] == -1 then Left with [j] = i else Left
              in (L_new, Right, i, j + 1)
            else
              let R_new = if i >= a && Right[i] == -1 then Right with [i] = j else Right
              in (Left, R_new, i - 1, j)
          in (Left_next, Right_next, i_next, j_next)
    in (L_final, R_final)

-- Find left and right match of a representative using the tree
let findLeftRightMatch 
    (tree: mintree.tree) (i: i64) 
    : (i64, i64) =
  let l = mintree.previous tree i
  let r = mintree.next tree i
  in (l, r)

-- Adjacent merge for a block (stack-based nearest smaller scan)
let adjacentMergeBOTH [n] (A: []i64) (L:[n]i64) (R:[n]i64) (offset: i64) : ([n]i64, [n]i64) =
  let (L_final, _, _) =
    loop (L_acc,top,stack) = (copy L, -1i64, replicate n 0i64) for i in offset ... (offset + n - 1) do
      let rec_top = loop top_acc = top while top_acc >= 0 && A[stack[top_acc]] > A[i] do top_acc - 1
      let L_acc2 = if rec_top >= 0 then L_acc with [i-offset] = stack[rec_top] else L_acc
      let top2 = rec_top + 1
      let stack2 = stack with [top2] = i
      in (L_acc2, top2, stack2)
  in

  -- Backward scan for R (next smaller)
  let (R_final, _, _) =
    loop (R_acc,top,stack) = (copy R, -1i64, replicate n 0i64) for i in (offset + n - 1)..(offset +n-2) ... (offset) do
      let rec_top = loop top_acc = top while top_acc >= 0 && A[stack[top_acc]] > A[i] do top_acc - 1
      let R_acc2 = if rec_top >= 0 then R_acc with [i - offset] = stack[rec_top] else R_acc
      let top2 = rec_top + 1
      let stack2 = stack with [top2] = i
      in (R_acc2, top2, stack2)
  in
  (L_final, R_final)


-- entries

entry findRepresentative_entry [n] (A: [n]i64) : i64 =
  findRepresentative A

entry farAwayBlocks_entry [n]
  (A: []i64) (a: i64) (b: i64) (c: i64) (d: i64)
  (L: [n]i64) (R: [n]i64) : ([n]i64, [n]i64) =
  farAwayBlocks_ANSV_linear A a b c d L R

entry findLeftRightMatch_entry (A: []i64) (i: i64) : []i64 =
  let tree = mintree.make A
  let (l,r) = findLeftRightMatch tree i
  in [l,r]

entry adjacentMerge_entry [n] (A: []i64) (L: [n]i64) (R: [n]i64) (offset: i64) : ([n]i64, [n]i64) =
  adjacentMergeBOTH A L R offset



-- ==
-- entry: findRepresentative_entry
-- input {[5i64,2i64,7i64,2i64,9i64]}
-- output {1i64}
-- input {[3i64,1i64,4i64,0i64]}
-- output {3i64}


-- ==
-- entry: adjacentMerge_entry
-- input {[5i64,2i64,6i64,1i64,4i64] [-1i64,-1i64,-1i64,-1i64,-1i64] [-1i64,-1i64,-1i64,-1i64,-1i64] 0i64}
-- output {[-1i64,-1i64,1i64,-1i64,3i64] [1i64,3i64,3i64,-1i64,-1i64]}
-- input {[1i64,2i64,3i64,4i64] [-1i64,-1i64,-1i64,-1i64] [-1i64,-1i64,-1i64,-1i64] 0i64}
-- output {[-1i64,0i64,1i64,2i64] [-1i64,-1i64,-1i64,-1i64]}
-- input {[4i64,3i64,2i64,1i64] [-1i64,-1i64,-1i64,-1i64] [-1i64,-1i64,-1i64,-1i64] 0i64}
-- output {[-1i64,-1i64,-1i64,-1i64] [1i64,2i64,3i64,-1i64]}
-- input {[42i64] [-1i64] [-1i64] 0i64}
-- output {[-1i64] [-1i64]}
-- input {[5i64,4i64,3i64,4i64,5i64] [-1i64,-1i64,-1i64,-1i64,-1i64] [-1i64,-1i64,-1i64,-1i64,-1i64] 0i64}
-- output {[-1i64,-1i64,-1i64,2i64,3i64] [1i64,2i64,-1i64,-1i64,-1i64]}
-- input {[9i64,8i64,5i64,6i64,7i64] [-1i64,-1i64,-1i64] [-1i64,-1i64,-1i64] 2i64}
-- output {[-1i64,2i64,3i64] [-1i64,-1i64,-1i64]}



-- ==
-- entry: findLeftRightMatch_entry
-- input {[5i64,2i64,6i64,1i64,4i64] 2i64}
-- output {[1i64,3i64]}
-- input {[8i64,6i64,7i64,5i64,3i64,4i64] 2i64}
-- output {[1i64,3i64]}


-- ==
-- entry: farAwayBlocks_entry
-- input {[8i64,6i64,7i64,5i64,3i64,4i64] 0i64 2i64 3i64 5i64 [-1i64,-1i64,-1i64,-1i64,-1i64,-1i64] [-1i64,76i64,-1i64,-1i64,-1i64,-1i64]}
-- output {[-1i64,-1i64,-1i64,-1i64,-1i64,-1i64] [3i64,76i64,3i64,-1i64,-1i64,-1i64]}
-- input {[5i64,3i64] 0i64 0i64 1i64 1i64 [-1i64,-1i64] [-1i64,-1i64]}
-- output {[-1i64,-1i64] [1i64,-1i64]}
-- input {[1i64,2i64,3i64,10i64,11i64] 0i64 2i64 3i64 4i64 [-1i64,-1i64,-1i64,-1i64,-1i64] [-1i64,-1i64,-1i64,-1i64,-1i64]}
-- output {[-1i64,-1i64,-1i64,2i64,2i64] [-1i64,-1i64,-1i64,-1i64,-1i64]}
-- input {[10i64,9i64,8i64,1i64,2i64] 0i64 2i64 3i64 4i64 [-1i64,-1i64,-1i64,-1i64,-1i64] [-1i64,-1i64,-1i64,-1i64,-1i64]}
-- output {[-1i64,-1i64,-1i64,-1i64,-1i64] [3i64,3i64,3i64,-1i64,-1i64]}
-- input {[5i64,1i64,4i64,2i64,3i64] 0i64 2i64 3i64 4i64 [-1i64,-1i64,-1i64,-1i64,-1i64] [-1i64,-1i64,-1i64,-1i64,-1i64]}
-- output {[-1i64,-1i64,-1i64,1i64,1i64] [-1i64,-1i64,3i64,-1i64,-1i64]}
-- input {[3i64,3i64,3i64,3i64] 0i64 1i64 2i64 3i64 [-1i64,-1i64,-1i64,-1i64] [-1i64,-1i64,-1i64,-1i64]}
-- output {[-1i64,-1i64,-1i64,-1i64] [2i64,2i64,-1i64,-1i64]}




