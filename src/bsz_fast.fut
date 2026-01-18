import "transparent_reduction_tree"


let BSZ [n] (A: [n]i64) (k: i64) : [n]i64 =


  let block_size = n/k
  let A = A :> [k * block_size]i64
  let B = unflatten A

  -- Make mintrees in parallel
  let trees = map (\b -> transparent_reduction_tree.make i64.min i64.highest b ) B

  let R_flat =
    map (\p ->
      let t = trees[p / block_size]
      let i = p % block_size
      in transparent_reduction_tree.previous (<) t i)
    (iota n)


  let block_mins = map (\t -> (transparent_reduction_tree.to_array t)[0] ) trees

  -- Fixes indices and calculates correct indices across blocks
  in map2 (\idx x ->
      if x != -1i64
        then ((idx / block_size) * block_size) + x
        else
          let b = idx / block_size
          let v = A[idx]
          let (block, found) = loop (block, found) = (b - 1, false)
            while block >= 0 && !found do
              let found' = block_mins[block] < v
              let block = if found'
                then block
                else block -1
            in (block, found')

          in if found
            then
              let value = (loop (i, result) = (block*block_size + block_size - 1, -1i64) while i >= 0 && result == -1i64 do
                let result' = if A[i] < v then i else result
                in (i - 1, result')).1
              in value

            else -1

        ) (iota n) R_flat



-- ==
-- entry: test_BSZ
-- compiled nobench input {
--  [1i64, 2i64, 3i64, 4i64] 2i64
-- }
-- output {
--  [-1i64, 0i64, 1i64, 2i64]
-- }
--
-- compiled nobench input {
--  [42i64] 1i64
-- }
-- output {
--  [-1i64]
-- }
--
-- compiled nobench input {
--  [4i64, 3i64, 2i64, 1i64] 2i64
-- }
-- output {
--  [-1i64, -1i64, -1i64, -1i64]
-- }
--
-- compiled nobench input {
--  [7i64, 7i64, 7i64, 7i64] 2i64
-- }
-- output {
--  [-1i64, -1i64, -1i64, -1i64]
-- }
--
-- compiled nobench input {
--  [2i64, 1i64, 2i64, 2i64] 2i64
-- }
-- output {
--  [-1i64, -1i64, 1i64, 1i64]
-- }
--
-- compiled nobench input {
--  [3i64, 1i64, 4i64, 2i64, 5i64, 6i64] 2i64
-- }
-- output {
--  [-1i64, -1i64, 1i64, 1i64, 3i64, 4i64]
-- }
--
-- compiled nobench input {
--  [5i64, 2i64, 8i64, 6i64, 3i64, 7i64, 1i64, 4i64] 2i64
-- }
-- output {
--  [-1i64, -1i64, 1i64, 1i64, 1i64, 4i64, -1i64, 6i64]
-- }
--
-- compiled nobench input {
--  [-1i64, -3i64, -2i64, 0i64] 2i64
-- }
-- output {
--  [-1i64, -1i64, 1i64, 2i64]
-- }
entry test_BSZ [n] (A: [n]i64) (k: i64) : [n]i64 =
        BSZ A k