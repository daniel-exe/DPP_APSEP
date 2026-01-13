import "reduction_tree"


module mintree = mk_mintree {
    type t = i32
    let highest = 2147483647i32
    let min (x: i32) (y: i32) : i32 = if x < y then x else y
    let (<=) (x: i32) (y: i32) : bool = x <= y
    let (<)  (x: i32) (y: i32) : bool = x < y
}


let SEQ [n] (A: [n]i32) : [n]i64 =
    let t = mintree.make A
    in map (\i -> mintree.strict_previous t i) (iota n)


let BSZ [n] (A: [n]i32) (k: i64) : [n]i64 =


    let num_blocks = n/k
    let A = A :> [num_blocks*k]i32

    -- Split input array up
    let B = unflatten A

    -- Sequentially find matches in each block
    let R_local = map SEQ B
    let R_temp = flatten R_local :> [n]i64

    let t = mintree.make A

    -- Fixes indices and calculates correct indices across blocks
    in map2 (\idx x ->
            if x != -1i64
            then ((idx / k) * k) + x
            else mintree.strict_previous t idx
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