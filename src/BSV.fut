-- Berkman & Vishkin ANSV skeleton in Futhark

import "reduction_tree_test"  -- or wherever your binary reduction tree modules are

-- Compute representative (smallest index in a block)
let findRepresentative [n] (arr: [n]i64) : i64 =
  reduce (\i1 i2 -> if arr[i2] < arr[i1] then i2 else i1) 0 (iota n)

-- Far-away merging (linear merge of blocks)
let farAwayBlocks_ANSV_linear [n] [m]
  (A: []i64) (a: i64) (b: i64) (c: i64) (d: i64)
  (L: [n]i64) (R: [m]i64) : ([n]i64, [m]i64) =
  if a > b || c > d || a == -1 || d == -1 then (L, R)
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


-- Find left and right match of a representative using the tree
let findLeftRightMatch 
    (tree: mintree.tree) (i: i64) 
    : (i64, i64) =
  let l = mintree.strict_previous tree i
  let r = mintree.strict_next tree i
  in (l, r)

-- Adjacent merge for a block (stack-based nearest smaller scan)
let adjacentMergeGeneric [n] (A: []i64) (X: [n]i64) (offset: i64) (forward: bool) : [n]i64 =
  let (start, step, end) =
    if forward then (offset, offset+1, offset + n - 1) else (offset + n - 1, offset + n - 2, offset)
  in
  let (X,_,_) = loop (X_acc, top, stack) = (copy X, -1i64, replicate n 0i64) for i in start .. step ... end do
    let rec_top = loop t = top while t >= 0 && A[stack[t]] > A[i] do t - 1
    let X_acc2 = if rec_top >= 0 && A[stack[rec_top]] < A[i] then X_acc with [i - offset] = stack[rec_top] else X_acc
    let top2 = rec_top + 1
    let stack2 = stack with [top2] = i
    in (X_acc2, top2, stack2)
  in X
let adjacentMergeBOTH [n] (A: []i64) (L:[n]i64) (R:[n]i64) (offset: i64) : ([n]i64, [n]i64) =
  (adjacentMergeGeneric A L offset true,  adjacentMergeGeneric A R offset false)


let ANSV_Berkman [n] (A: [n]i64) (blockSize: i64) : ([n]i64, [n]i64) =

  -- Step 1: Create min-binary tree
  let tree = mintree.make A

  let blockCount = (n + blockSize - 1) / blockSize
  
  -- Step 1: local block processing
  let (L_blocks, R_blocks, REPs, B_blocks) =
    iota blockCount
    |> map (\blockNumber ->
        let start = blockNumber * blockSize
        let len = i64.min blockSize (n - start)
        let L_block = replicate blockSize (-1i64)
        let R_block = replicate blockSize (-1i64)
        let (L_block, R_block) =
          let (l, r) = adjacentMergeBOTH A (copy L_block[0:len]) (copy R_block[0:len]) start
        in (L_block with [0:len] = copy l, R_block with [0:len] = copy r)
        let block = A[start:(start+len)] 
        let ri = start + findRepresentative block
        let (b1, b2) = findLeftRightMatch tree ri

        in (L_block, R_block, ri, (b1, b2))
      )
    |> unzip4

  let L0 = flatten L_blocks
  let R0 = flatten R_blocks
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
          if BLi >= 0 && BLi + 1 == BCi && b1 != -1 then
            -- interval [b1 .. ri] inclusive => length = ri - b1 + 1, offset = b1
            let off = b1
            let len = i64.max 0 (ri - b1 + 1)
            let len = i64.min len (n - off)     -- clip to array end
            let (l, r) = adjacentMergeBOTH A (copy L_acc[off:off+len]) (copy R_acc[off:off+len]) off
            in (copy L_acc with [off:off+len] = copy l, copy R_acc with [off:off+len] = copy r)
          else (L_acc, R_acc)

        let (L2, R2) =
          if BRi >= 0 && BRi - 1 == BCi && b2 != -1 then
            -- interval [ri .. b2] inclusive => length = b2 - ri + 1, offset = ri
            let off = ri
            let len = i64.max 0 (b2 - ri + 1)
            let len = i64.min len (n - off)
            let (l, r) = adjacentMergeBOTH A (copy L1[off:off+len]) (copy R1[off:off+len]) off
            in (copy L1 with [off:off+len] = copy l, copy R1 with [off:off+len] = copy r)
          else (L1, R1)


        -- Far-away merge: call linear merging that requires arrays with known size
        let (L3, R3) =
          if b1 != -1 && b2 != -1 then
            let ((_,bBL), (bBR,_)) = (B_blocks[BLi], B_blocks[BRi])
            let (L_temp, R_temp) =
              if BLi == bBR / blockSize then
                farAwayBlocks_ANSV_linear A bBR (b1) b2 (rBR) (copy L2) (copy R2)
              else (L2, R2)
            let (L4, R4) =
              if BRi == bBL / blockSize then
                farAwayBlocks_ANSV_linear A rBL (b1) b2 (bBL) (copy L_temp) (copy R_temp)
              else (L_temp, R_temp)
            in (L4, R4)
          else (L2, R2)
        in (L3, R3)
        --in (L2, R2)
  let Lf = L_final[0:n]
  let Rf = R_final[0:n]
  in (Lf, Rf)


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

entry ANSV_Berkman_entry (A: []i64) (blockSize: i64) : ([]i64, []i64) =
  ANSV_Berkman A blockSize


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
-- input {[9i64,9i64,9i64,9i64,9i64] [-1i64,-1i64,-1i64] [-1i64,-1i64,-1i64] 2i64}
-- output {[-1i64,-1i64,-1i64] [-1i64,-1i64,-1i64]}
-- input {[5i64,1i64,9i64,10i64,4i64,6i64] [-1i64,1i64,-1i64,-1i64] [-1i64,-1i64,4i64,-1i64] 1i64}
-- output {[-1i64,1i64,2i64,1i64] [-1i64,4i64,4i64,-1i64] }

-- ==
-- entry: findLeftRightMatch_entry
-- input {[5i64,1i64,9i64,10i64,4i64,6i64] 4i64}
-- output {[1i64,-1i64]}
-- input {[8i64,6i64,7i64,5i64,3i64,4i64] 2i64}
-- output {[1i64,3i64]}


-- ==
-- entry: farAwayBlocks_entry
-- input {[8i64,6i64,7i64,5i64,3i64,4i64] 0i64 2i64 3i64 5i64 [-1i64,-1i64,-1i64] [-1i64,76i64,-1i64]}
-- output {[-1i64,-1i64,-1i64,] [3i64,76i64,3i64]}
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


-- ==
-- entry: ANSV_Berkman_entry
-- input {[5i64,1i64,9i64,10i64,4i64,6i64] 2i64}
-- output {[-1i64,-1i64,1i64,2i64,1i64,4i64] [1i64,-1i64,4i64,4i64,-1i64,-1i64]}
-- input {[3i64,1i64,4i64,2i64,5i64] 1i64}
-- output {[-1i64,-1i64,1i64,1i64,3i64] [1i64,-1i64,3i64,-1i64,-1i64]}
-- input {[3i64,1i64,2i64,-1i64,6i64] 2i64}
-- output {[-1i64,-1i64,1i64,-1i64,3i64] [1i64,3i64,3i64,-1i64,-1i64]}
-- input {[3i64,1i64,4i64,5i64] 2i64}
-- output {[-1i64,-1i64,1i64,2i64] [1i64,-1i64,-1i64,-1i64]}
-- input {[2i64,1i64,3i64,4i64] 2i64}
-- output {[-1i64,-1i64,1i64,2i64] [1i64,-1i64,-1i64,-1i64]}
-- input {[4i64,3i64,1i64,2i64] 2i64}
-- output {[-1i64,-1i64,-1i64,2i64] [1i64,2i64,-1i64,-1i64]}
-- input {[3i64,2i64,1i64,4i64] 2i64}
-- output {[-1i64,-1i64,-1i64,2i64] [1i64,2i64,-1i64,-1i64]}
-- input {[2i64,2i64,2i64,2i64] 2i64}
-- output {[-1i64,-1i64,-1i64,-1i64] [-1i64,-1i64,-1i64,-1i64]}
-- input {[7i64,6i64,5i64,8i64,9i64] 3i64}
-- output {[-1i64,-1i64,-1i64,2i64,3i64] [1i64,2i64,-1i64,-1i64,-1i64]}
-- input {[1i64,2i64,3i64,4i64,5i64] 2i64}
-- output {[-1i64,0i64,1i64,2i64,3i64] [-1i64,-1i64,-1i64,-1i64,-1i64]}
-- input {[5i64,4i64,3i64,2i64,1i64] 2i64}
-- output {[-1i64,-1i64,-1i64,-1i64,-1i64] [1i64,2i64,3i64,4i64,-1i64]}
-- input {[4i64,1i64,3i64,2i64] 2i64}
-- output {[-1i64,-1i64,1i64,1i64] [1i64,-1i64,3i64,-1i64]}
-- input {[1i64] 2i64}
-- output {[-1i64] [-1i64]}
-- input {[1i64,2i64,3i64,4i64,5i64] 2i64}
-- output {[-1i64,0i64,1i64,2i64,3i64] [-1i64,-1i64,-1i64,-1i64,-1i64]}
-- input {[5i64,4i64,3i64,2i64,1i64] 2i64}
-- output {[-1i64,-1i64,-1i64,-1i64,-1i64] [1i64,2i64,3i64,4i64,-1i64]}
-- input {[2i64,2i64,2i64,2i64] 2i64}
-- output {[-1i64,-1i64,-1i64,-1i64] [-1i64,-1i64,-1i64,-1i64]}
-- input {[5i64,1i64,2i64,3i64] 2i64}
-- output {[-1i64,-1i64,1i64,2i64] [1i64,-1i64,-1i64,-1i64]}
-- input {[2i64,3i64,1i64,4i64] 2i64}
-- output {[-1i64,0i64,-1i64,2i64] [2i64,2i64,-1i64,-1i64]}
-- input {[3i64,1i64,4i64,2i64,5i64,0i64] 2i64}
-- output {[-1i64,-1i64,1i64,1i64,3i64,-1i64] [1i64,5i64,3i64,5i64,5i64,-1i64]}
-- input {[6i64,5i64,7i64,4i64,9i64,2i64] 2i64}
-- output {[-1i64,-1i64,1i64,-1i64,3i64,-1i64] [1i64,3i64,3i64,5i64,5i64,-1i64]}







