-- Berkman & Vishkin ANSV skeleton in Futhark

import "reduction_tree_test"  -- or wherever your binary reduction tree modules are

-- Compute representative (smallest index in a block)
let findRepresentative (arr: []i64) (start: i64) (end_: i64) : i64 =
  let indices = iota (end_ - start) |> map (\i -> start + i)
  in reduce (\i1 i2 -> if arr[i2] < arr[i1] then i2 else i1) (indices[0]) (indices)

-- Far-away merging (linear merge of blocks)
let farAwayBlocks_ANSV_linear [n]
  (A: []i64) (a: i64) (b: i64) (c: i64) (d: i64)
  (L: [n]i64) (R: [n]i64) : ([n]i64, [n]i64) =
  if a == -1 || d == -1 then
    (L, R)
  else
    let b_i = b - 1
    -- Linear scan using while loop
    let (L_final, R_final, _, _) =
      loop (Left, Right, i, j) = (copy L, copy R, b_i, c) 
        while i >= a || j < (d - 1) do
          let (Left_next, Right_next, i_next, j_next) =
            if A[i] < A[j] then
              let L_new = if Left[j] == -1 then Left with [j] = i else Left
              in (L_new, Right, i, j + 1)
            else
              let R_new = if Right[i] == -1 then Right with [i] = j else Right
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
  let L = replicate n (-1i64)
  let R = replicate n (-1i64)

  -- Forward scan for L (previous smaller)
  let (L_final, _, _) =
    loop (L_acc,top,stack) = (L, -1i64, replicate n 0i64) for i in offset ... (offset + n - 1) do
      let rec_top = loop top_acc = top while top_acc >= 0 && A[stack[top_acc]] > A[i] do top_acc - 1
      let L_acc2 = if rec_top >= 0 then L_acc with [i] = stack[rec_top] else L_acc
      let top2 = rec_top + 1
      let stack2 = stack with [top2] = i
      in (L_acc2, top2, stack2)
  in

  -- Backward scan for R (next smaller)
  let (R_final, _, _) =
    loop (R_acc,top,stack) = (R, -1i64, replicate n 0i64) for i in offset + n - 1 ... (offset + n - 1) do
      let rec_top = loop top_acc = top while top_acc >= 0 && A[stack[top_acc]] > A[i] do top_acc - 1
      let R_acc2 = if rec_top >= 0 then R_acc with [i] = stack[rec_top] else R_acc
      let top2 = rec_top + 1
      let stack2 = stack with [top2] = i
      in (R_acc2, top2, stack2)
  in
  (L_final, R_final)


-- Main ANSV skeleton
let ANSV_Berkman (A: []i64) (blockSize: i64) : ([]i64, []i64) =
  let n = length A

  -- Step 1: Create min-binary tree
  let tree = mintree.make A

  -- Allocate output
  let L0 = replicate n (-1i64)
  let R0 = replicate n (-1i64)

  let blockCount = (n + blockSize - 1) / blockSize
  let REPs = replicate blockCount 0i64

  -- Step 1: local block processing
  let (L_blocks, R_blocks, REPs, B_blocks) =
    iota blockCount
    |> map (\blockNumber ->
        let start = blockNumber * blockSize
        let end_ = i64.min n ((blockNumber+1)*blockSize)
        let (L_block, R_block) = adjacentMergeBOTH A start
        let ri = findRepresentative A start end_
        let (b1, b2) = findLeftRightMatch tree ri
        in (L_block, R_block, ri, (b1, b2))
      )
    |> unzip4

  -- Step 2: nonlocal merging
  let (L_final, R_final) =
      loop (L_acc, R_acc) = (copy L0, copy R0) for BCi < blockCount do
        let ri = REPs[BCi]
        let (b1, b2) = B_blocks[BCi]
        let BLi = if b1 == -1 then -1 else b1 / blockSize
        let BRi = if b2 == -1 then -1 else b2 / blockSize
        let rBL = if BLi >= 0 then REPs[BLi] else -1
        let rBR = if BRi >= 0 then REPs[BRi] else -1

        -- Adjacent merge (operate on preallocated arrays L_acc, R_acc)
        let (L1, R1) =
          if BLi + 1 == BCi && b1 != -1 then
            adjacentMergeBOTH A b1
          else (L_acc, R_acc)

        let (L2, R2) =
          if BRi - 1 == BCi && b2 != -1 then
            adjacentMergeBOTH A ri
          else (L1, R1)

        -- Far-away merge: call linear merging that requires arrays with known size
        let (L3, R3) =
          if b1 != -1 && b2 != -1 then
            let ((bBL,_), (_,bBR)) = (B_blocks[2*BLi+1], B_blocks[2*BRi])
            let (L_temp, R_temp) =
              if BLi == bBR / blockSize then
                farAwayBlocks_ANSV_linear A bBR (b1+1) b2 (rBR+1) L2 R2
              else (L2, R2)
            let (L4, R4) =
              if BRi == bBL / blockSize then
                farAwayBlocks_ANSV_linear A rBL (b1+1) b2 (bBL+1) L_temp R_temp
              else (L_temp, R_temp)
            in (L4, R4)
          else (L2, R2)

        in (L3, R3)

    in (L_final, R_final)


-- entries

entry findRepresentative_entry (A: []i64) : i64 =
  findRepresentative A

entry farAwayBlocks_entry
  (A: []i64) (a: i64) (b: i64) (c: i64) (d: i64)
  (L: []i64) (R: []i64) : ([]i64, []i64) =
  farAwayBlocks_ANSV_linear A a b c d L R

entry findLeftRightMatch_entry (A: []i64) (i: i64) : []i64 =
  let mintree = mintree.make A
  in findLeftRightMatch tree i

entry adjacentMerge_entry (A: []i64) (offset: i64) (len: i64) : ([]i64, []i64) =
  adjacentMergeBOTH A offset len

entry ANSV_Berkman_entry (A: []i64) (blockSize: i64) : ([]i64, []i64) =
  ANSV_Berkman A blockSize



  -- ==
-- entry: findRepresentative_entry
-- input {[5i64, 2, 7, 2, 9i64]}
-- output {1i64}
-- input {[3i64, 1, 4, 0i64]}
-- output {3i64}

-- ==
-- entry: adjacentMerge_entry
-- input {[5i64, 2, 6, 1, 4i64] 0i64 5i64}
-- output {[ -1i64, -1i64, 1i64, -1i64, 3i64 ] [ 1i64, 3i64, 3i64, -1i64, -1i64 ]}
-- input {[1i64, 2, 3, 4i64] 0i64 4i64}
-- output {[ -1i64, 0i64, 1i64, 2i64 ] [ -1i64, -1i64, -1i64, -1i64 ]}
-- input {[4i64, 3, 2, 1i64] 0i64 4i64}
-- output {[ -1i64, -1i64, -1i64, -1i64 ] [ 1i64, 2i64, 3i64, -1i64 ]}

-- ==
-- entry: findLeftRightMatch_entry
-- input {[5i64, 2, 6, 1, 4i64] 2i64}
-- output {[1i64, 3i64]}
-- input {[8i64, 6, 7, 5, 3, 4i64] 2i64}
-- output {[1i64, 3i64]}

-- ==
-- entry: farAwayBlocks_entry
-- input {[8i64, 6, 7, 5, 3, 4i64] 0i64 3i64 3i64 6i64 [-1i64,-1,-1,-1,-1,-1] [-1i64,-1,-1,-1,-1,-1]}
-- output {[-1i64,-1i64,-1i64,0i64,1i64,2i64] [3i64,3i64,3i64,-1i64,-1i64,-1i64]}

-- ==
-- entry: ANSV_Berkman_entry
-- input {[5i64, 2, 6, 1, 4i64] 2i64}
-- output {[-1i64, -1i64, 1i64, -1i64, 3i64] [1i64, 3i64, 3i64, -1i64, -1i64]}