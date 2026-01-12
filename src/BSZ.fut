let process_block [n] (A: [n]i32) (R: []i32) (start: i64) (end: i64) : []i64 =
    -- TODO: Process A sequentially from start to end
    []


let BSZ [n] (A: [n]i32) (R: []i32) (k: i64) : [n]i64 =
    let A_rev = A[::-1]
    let num_blocks = (n + k - 1) / k
    let block_bounds = map (\i ->
            let start = i * k
            let end = i64.min (start + k - 1) (n - 1)
            in (start, end)
            ) (iota num_blocks)

    let Rs = map (\bounds ->
            let start = bounds.0
            let end = bounds.1
            in process_block A_rev R start end
            ) block_bounds

    let block_map = map (\idx -> idx / k) (iota n)

    let R_rev = map (\idx ->
            let block_idx = block_map[idx]
            in Rs[block_idx][idx]
            ) (iota n)

    in map (\i ->
        let j = R_rev[n - 1 - i]
        in if j == -1
           then -1
           else (n - 1 - j)
        ) (iota n)



