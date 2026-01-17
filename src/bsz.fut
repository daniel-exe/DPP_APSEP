import "transparent_reduction_tree"


let BSZ [n] (A: [n]i32) (k: i64) : [n]i64 =


    let block_size = n/k
    let A = A :> [k*block_size]i32

    -- Split input array up
    let B = unflatten A

    -- Make mintrees in parallel
    let trees = map (\b -> transparent_reduction_tree.make i32.min i32.highest b ) B

    let R_local = map (\t -> map (\i -> transparent_reduction_tree.previous (<) t i) (iota block_size)) trees

    let R_temp = flatten R_local :> [n]i64

    let t = transparent_reduction_tree.make i32.min i32.highest A

    -- Fixes indices and calculates correct indices across blocks
    in map2 (\idx x ->
            if x != -1i64
            then ((idx / block_size) * block_size) + x
            else transparent_reduction_tree.previous (<) t idx
        ) (iota n) R_temp



-- ==
-- entry: test_BSZ
-- compiled nobench input {
--  [1i32, 2i32, 3i32, 4i32] 2i64
-- }
-- output {
--  [-1i64, 0i64, 1i64, 2i64]
-- }
--
-- compiled nobench input {
--  [42i32] 1i64
-- }
-- output {
--  [-1i64]
-- }
--
-- compiled nobench input {
--  [4i32, 3i32, 2i32, 1i32] 2i64
-- }
-- output {
--  [-1i64, -1i64, -1i64, -1i64]
-- }
--
-- compiled nobench input {
--  [7i32, 7i32, 7i32, 7i32] 2i64
-- }
-- output {
--  [-1i64, -1i64, -1i64, -1i64]
-- }
--
-- compiled nobench input {
--  [2i32, 1i32, 2i32, 2i32] 2i64
-- }
-- output {
--  [-1i64, -1i64, 1i64, 1i64]
-- }
--
-- compiled nobench input {
--  [3i32, 1i32, 4i32, 2i32, 5i32, 6i32] 2i64
-- }
-- output {
--  [-1i64, -1i64, 1i64, 1i64, 3i64, 4i64]
-- }
--
-- compiled nobench input {
--  [5i32, 2i32, 8i32, 6i32, 3i32, 7i32, 1i32, 4i32] 2i64
-- }
-- output {
--  [-1i64, -1i64, 1i64, 1i64, 1i64, 4i64, -1i64, 6i64]
-- }
--
-- compiled nobench input {
--  [-1i32, -3i32, -2i32, 0i32] 2i64
-- }
-- output {
--  [-1i64, -1i64, 1i64, 2i64]
-- }
entry test_BSZ [n] (A: [n]i32) (k: i64) : [n]i64 =
        BSZ A k