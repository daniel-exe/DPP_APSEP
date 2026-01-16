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
          if (i >= a) && (j <= d) then
            if A[i] < A[j] then
              let Left'  = if Left[j-c] == -1 then Left  with [j-c] = i else Left
              in (Left', Right, i, j + 1)
            else
              let Right' = if Right[i-a] == -1 then Right with [i-a] = j else Right
              in (Left, Right', i - 1, j)
          else if j <= d then
            (Left, Right, i, j + 1)
          else
            (Left, Right, i - 1, j)
    in (Lf, Rf)

-- Find left and right match of a representative using the tree
let findLeftRightMatch (tree: mintree.tree) (i: i64) : (i64, i64) =
  let l = mintree.strict_previous tree i
  let r = mintree.strict_next tree i
  in (l, r)

-- Adjacent merge for a block (stack-based nearest smaller scan)
let adjacentMergeGeneric [n] (A: []i64) (X: [n]i64) (offset: i64) (forward: bool) : [n]i64 =
  let max = i64.min (offset + n - 1) (length A - 1)
  let (start, step, end) = if forward then (offset, offset+1, max ) else (max , max-1, offset)
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

  let tree = mintree.make A
  let blockCount = (n + blockSize - 1) / blockSize
  let blocks = iota blockCount
  let (L_blocks, R_blocks, REPs, B_blocks) =
    blocks
    |> map (\blockNumber ->
        let start = blockNumber * blockSize
        let len = i64.min blockSize (n - start)
        let (L_block, R_block) = adjacentMergeBOTH A (replicate blockSize (-1i64)) (replicate blockSize (-1i64)) start
        let block = A[start:(start+len)] 
        let ri = start + findRepresentative block
        let (b1, b2) = findLeftRightMatch tree ri
        in (L_block, R_block, ri, (b1, b2))
      )
    |> unzip4

  let L0 = (flatten L_blocks)[0:n]
  let R0 = (flatten R_blocks)[0:n]

  let bfsize = (2 * blockSize)
  let Ix = replicate bfsize (-1)
  let (L1, R1, I1) = map3 (\ri (b1,b2) bl-> 
    let BLi = if b1 == -1 then -1 else b1 / blockSize
    in if BLi >= 0 && BLi + 1 == bl && b1 != -1 then   
        let len = i64.min (i64.max 0 (ri - b1 + 1)) (n - b1)
        let I = map (\x -> if x < len && x != 0 then x + b1 else -1) (iota bfsize)
        let (l, r) = adjacentMergeBOTH A (copy L0[b1:b1+len]) (copy R0[b1:b1+len]) b1 
        in ((replicate bfsize (-1)) with [0:len] = l, (replicate bfsize (-1)) with [0:len] = r, I)
    else (Ix,Ix,Ix)
    ) REPs B_blocks blocks |> unzip3  
    
  let I1 = (flatten I1)
  let L1 = scatter (copy L0) I1 (flatten L1)
  let R1 = scatter (copy R0) I1 (flatten R1)

  let (L2, R2, I2) = map3 (\ri (b1,b2) bl-> 
    let BRi = if b2 == -1 then -1 else b2 / blockSize 
    in if BRi >= 0 && BRi - 1 == bl && b2 != -1 then
        let len = i64.min (i64.max 0 (b2-ri + 1)) (n - ri)
        let I = map (\x -> if x < (len-1) then x + ri else -1) (iota bfsize)
        let (l, r) = adjacentMergeBOTH A (copy L1[ri:ri+len]) (copy R1[ri:ri+len]) ri 
        in ((replicate bfsize (-1)) with [0:len] = l, (replicate bfsize (-1)) with [0:len] = r, I)
    else (Ix,Ix,Ix)
    ) REPs B_blocks blocks |> unzip3  
  
  let I2 = (flatten I2)
  let L2 = scatter (copy L1) I2 (flatten L2)
  let R2 = scatter (copy R1) I2 (flatten R2)
  
  let In = (replicate n (-1))
  let (L3, R3, I3) = map3 (\ri (b1,b2) bl-> 
    if b1 != -1 && b2 != -1 then
      let BLi = if b1 == -1 then -1 else b1 / blockSize
      let BRi = if b2 == -1 then -1 else b2 / blockSize 
      let rBR = if BRi >= 0 then REPs[BRi] else -1
      let ((_,bBL), (bBR,_)) = (B_blocks[BLi], B_blocks[BRi]) in 
      if BLi == bBR / blockSize then
        let (l,r) = farAwayBlocks_ANSV_linear A bBR (b1) b2 (rBR) (copy L2[b2:rBR+1]) (copy R2[bBR+1:b1+1]) 
        let I = map (\x -> if (x > bBR && x <= b1 ) || (x >= b2 && x <= rBR) then x else -1) (iota n)
        in ((replicate n (-1)) with[b2:rBR+1] = l, (replicate n (-1)) with [bBR+1:b1+1] = r, I)
      else (In,In ,In )   
    else (In,In ,In ) 
      ) REPs B_blocks blocks |> unzip3  

  let I3 = (flatten I3)
  let L3 = scatter (copy L2) I3 (flatten L3)
  let R3 = scatter (copy R2) I3 (flatten R3)


  let (L4, R4, I4) = map3 (\ri (b1,b2) bl-> 
    if b1 != -1 && b2 != -1 then
      let BLi = if b1 == -1 then -1 else b1 / blockSize
      let BRi = if b2 == -1 then -1 else b2 / blockSize 
      let rBL = if BLi >= 0 then REPs[BLi] else -1
      let ((_,bBL), (bBR,_)) = (B_blocks[BLi], B_blocks[BRi]) in 
      if BRi == bBL / blockSize then
        let (l,r) = farAwayBlocks_ANSV_linear A rBL (b1) b2 (bBL) (copy L2[b2:bBL]) (copy R2[rBL:b1+1]) 
        let I = map (\x -> if (x >= rBL && x <= b1) || (x >= b2 && x < bBL ) then x else -1) (iota n)
        in ((replicate n (-1)) with[b2:bBL] = l, (replicate n (-1)) with [rBL:b1+1] = r, I)
      else (In,In ,In )   
    else (In,In ,In ) 
      ) REPs B_blocks blocks |> unzip3  

  let I4 = (flatten I4)
  let L4 = scatter (copy L3) I4 (flatten L4)
  let R4 = scatter (copy R3) I4 (flatten R4)
  in (L1, R1)


entry findRepresentative_entry [n] (A: [n]i64) : i64 =
  findRepresentative A


entry findLeftRightMatch_entry (A: []i64) (i: i64) : []i64 =
  let tree = mintree.make A
  let (l,r) = findLeftRightMatch tree i
  in [l,r]


entry ANSV_Berkman_entry (A: []i64) (blockSize: i64) : ([]i64, []i64) =
  ANSV_Berkman A blockSize

entry adjacentMerge_entry [n] (A: []i64) (L: [n]i64) (R: [n]i64) (offset: i64) : ([n]i64, [n]i64) =
  adjacentMergeBOTH A L R offset

entry farAwayBlocks_entry [n]
  (A: []i64) (a: i64) (b: i64) (c: i64) (d: i64)
  (L: [n]i64) (R: [n]i64) : ([n]i64, [n]i64) =
  farAwayBlocks_ANSV_linear A a b c d L R


-- ==
-- entry: ANSV_Berkman_entry
-- input {[0i64,5i64,1i64,0i64,0i64,0i64] 2i64}
-- output {[-1i64,0i64,0i64,-1i64,-1i64,-1i64][-1i64,2i64,3i64,-1i64,-1i64,-1i64]}

-- input {[3i64,1i64  ,4i64,2i64  ,5i64,0i64] 2i64}
-- output {[-1i64,-1i64,1i64,1i64,3i64,-1i64] [1i64,5i64,3i64,5i64,5i64,-1i64]}
-- input {[6i64,5i64,7i64,4i64,9i64,2i64] 2i64}
-- output {[-1i64,-1i64,1i64,-1i64,3i64,-1i64] [1i64,3i64,3i64,5i64,5i64,-1i64]}

-- if block(right smaller of left smaller) == block(right smaller)
-- if block(left smaller of right smaller) == block(left smaller)

-- input {[3i64,1i64   ,4i64,2i64   ,5i64,0i64] 2i64}
-- output {[-1i64,-1i64,1i64,1i64,3i64,-1i64] [1i64,-1i64,3i64,5i64,5i64,-1i64]}
-- input {[6i64,5i64  ,7i64,4i64  ,9i64,2i64] 2i64}
-- output {[-1i64,-1i64,1i64,-1i64,3i64,-1i64] [1i64,3i64,3i64,5i64,5i64,-1i64]}
-- only if smaller of rep is in right [3-5] ![1-3,3-5] ERR     

 
-- input {[3i64,1i64,4i64,2i64,5i64,0i64] 2i64}
-- output {[-1i64,-1i64,1i64,1i64,-1i64,-1i64] [1i64,-1i64,3i64,-1i64,5i64,-1i64]}
-- input {[6i64,5i64,7i64,4i64,9i64,2i64] 2i64}
-- output {[-1i64,-1i64,-1i64,-1i64,-1i64,-1i64] [1i64,-1i64,3i64,-1i64,5i64,-1i64]}
-- only if smaller of rep is in left. [1-3] []

-- input {[3i64,1i64,4i64,2i64,5i64,0i64] 2i64}
-- output {[-1i64,-1i64,-1i64,-1i64,-1i64,-1i64] [1i64,-1i64,3i64,-1i64,5i64,-1i64]}
-- input {[6i64,5i64,7i64,4i64,9i64,2i64] 2i64}
-- output {[-1i64,-1i64,-1i64,-1i64,-1i64,-1i64] [1i64,-1i64,3i64,-1i64,5i64,-1i64]}
-- within blocks only

-- input {[3i64,1i64,4i64,2i64,5i64,0i64] 2i64}
-- output {[-1i64,-1i64,1i64,1i64,3i64,-1i64] [1i64,5i64,3i64,5i64,5i64,-1i64]}
-- input {[6i64,5i64,7i64,4i64,9i64,2i64] 2i64}
-- output {[-1i64,-1i64,1i64,-1i64,3i64,-1i64] [1i64,3i64,3i64,5i64,5i64,-1i64]}






