import "reduction_tree2"


module mintree = mk_mintree i32


let SEQ [n] (A: [n]i32) : ([n]i64, mintree.tree [n]) =
    let t = mintree.make A
    in  (map (\i -> mintree.strict_previous t i) (iota n), t)



let BSZ [n] (A: [n]i32) (k: i64) : [n]i64 =


    let block_size = n/k
    let A = A :> [k*block_size]i32

    -- Split input array up
    let B : [k][block_size]i32 = unflatten A

    -- Make mintrees in parallel
    let (R_local, trees) = unzip (map SEQ B)

    let R_temp = flatten R_local :> [n]i64

    let block_mins = map mintree.minimum trees

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