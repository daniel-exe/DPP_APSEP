import "transparent_reduction_tree"


let BSZ [n] (A: [n]i64) (k: i64) : [n]i64 =
  let block_size = n/k
  let A = A :> [k*block_size]i64
  let B = unflatten A

    -- Make mintrees in parallel
  let trees = map (\b -> transparent_reduction_tree.make i64.min i64.highest b ) B

  let R_local = map (\t -> map (\i -> transparent_reduction_tree.previous (<) t i) (iota block_size)) trees

  let R_temp = flatten R_local :> [n]i64

  let t = transparent_reduction_tree.make i64.min i64.highest A

    -- Fixes indices and calculates correct indices across blocks
  in map2 (\idx x ->
    if x != -1i64
      then ((idx / block_size) * block_size) + x
      else transparent_reduction_tree.previous (<) t idx
    ) (iota n) R_temp



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