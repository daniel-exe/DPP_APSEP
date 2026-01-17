-- # Tree operations.
--
-- We import a library so we don't have to write a segmented scan
-- ourselves. Remember to run `futhark pkg sync` to download it.
import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/diku-dk/sorts/merge_sort" 

-- A traversal is an array of these steps.
type step = #u | #d i32

-- ## Input handling.
--
-- You do not have to modify this. The function 'input.steps' takes as
-- argument a string with steps as discussed in the assignment text,
-- and gives you back an array of type '[]step'.
--
-- Example:
--
-- ```
-- > input.steps "d0 d2 d3 u u d5 u"
-- [#d 0, #d 2, #d 3, #u, #u, #d 5, #u]
-- ```

type char = u8
type string [n] = [n]char

module input
  : {
      -- | Parse a string into an array of commands.
      val steps [n] : string [n] -> []step
    } = {
  def is_space (x: char) = x == ' ' || x == '\n'
  def isnt_space x = !(is_space x)

  def (&&&) f g = \x -> (f x, g x)

  def dtoi (c: char) : i32 = i32.u8 c - '0'

  def is_digit (c: char) = c >= '0' && c <= '9'

  def atoi [n] (s: string [n]) : i32 =
    let (sign, s) = if n > 0 && s[0] == '-' then (-1, drop 1 s) else (1, s)
    in sign
       * (loop (acc, i) = (0, 0)
          while i < length s do
            if is_digit s[i]
            then (acc * 10 + dtoi s[i], i + 1)            else (acc, n)).0

  def to_step (s: []char) : step =
    match s[0]
    case 'u' -> #u
    case _ -> #d (atoi (drop 1 s))

  type slice = (i64, i64)

  def get 't ((start, end): slice) (xs: []t) =
    xs[start:end]

  def words [n] (s: string [n]) : []slice =
    segmented_scan (+) 0 (map is_space s) (map (isnt_space >-> i64.bool) s)
    |> (id &&& rotate 1)
    |> uncurry zip
    |> zip (indices s)
    |> filter (\(i, (x, y)) -> (i == n - 1 && x > 0) || x > y)
    |> map (\(i, (x, _)) -> (i - x + 1, i + 1))

  def steps [n] (s: string [n]) =
    map (\slice -> to_step (get slice s)) (words s)
}

-- ## Task 2.1

def exscan f ne xs =
  map2 (\i x -> if i == 0 then ne else x)
       (indices xs)
       (rotate (-1) (scan f ne xs))

def depths (steps: []step) : [](i64, i32) =
  let is_dx   = map (\s -> match s case #d _ -> true case #u -> false) steps
  let values  = map (\s -> match s case #d v -> v case #u -> 0) steps  
  let delta   = map (\s -> match s case #d _ ->  1i64 case #u -> -1i64) steps
  let exclusive = exscan (+) 0i64 delta 

  let depths_nodes = map (\(d,_flag) -> d) (filter (\(_d,flag) -> flag) (zip exclusive is_dx))
  let values_nodes = map (\(v,_flag) -> v) (filter (\(_v,flag) -> flag) (zip values is_dx))
  let l = length depths_nodes
  let depths_nodes = depths_nodes :> [l]i64
  let values_nodes = values_nodes :> [l]i32

  let result : [] (i64, i32) = zip depths_nodes values_nodes
  in result


-- ## Task 2.2
def binary_search [n] 't (lte: t -> t -> bool) (xs: [n]t) (x: t) : i64 =
  let (l, _) =
    loop (l, r) = (0, n-1) while l < r do
      let t = l + (r - l + 1) / 2  -- note the +1 to bias right
      in if xs[t] `lte` x
         then (t, r)       
         else (l, t - 1)   
  in l

def parents (D: []i64) : []i64 = 
  let l = length D
  let D = D :> [l]i64
  let inds = iota(l)
  let pairs = zip D inds
  let sorted_pairs =
    merge_sort_by_key
      (\(d, idx) -> (d, idx))
      (\(a0, a1) (b0, b1) ->
          a0 < b0 || (a0 == b0 && a1 < b1))
      pairs
  let parentarr = map (\i -> if i == 0 then 0
                          else                                 
                              let idx = binary_search (\(a: (i64,i64)) (b: (i64,i64)) -> a.0 < b.0 || (a.0 == b.0 && a.1 < b.1)) (sorted_pairs) ((D[i] - 1i64), i)
                              in sorted_pairs[idx].1) inds
  in parentarr :> []i64 

-- ## Task 2.3
def subtree_sizes [n] (steps: [n]step) : []i64 =
  let (deps, vals) = unzip (depths steps)
  let l = length deps
  let maxdeps = i64.maximum deps

  let parents = (parents deps) :> [l]i64
  let inds = iota l
  let empty = replicate l 0

  let result =
    let (res, _) =
      loop (result, dep) = (empty, maxdeps) while dep >= 0 do
        let add1 = map (\i -> if deps[i] == dep then (i64.i32 vals[i]) else 0i64) inds
        let add1' = (map (\i -> if (deps[i] == dep && deps[i] != 0) then (i64.i32 vals[i]+result[i]) else 0i64) inds)
        let addparents = reduce_by_index (copy empty) (+) 0 parents add1'
        let add2 = scatter (copy empty) inds addparents
        let result' = map2 (+) result add1
        let result'' = map2 (+) result' add2
        in (result'', dep - 1)
    in res
  in result

entry Eparents = parents
entry Esubtree_sizes [n] (s: string [n])  :  []i64 = 
  subtree_sizes (input.steps s)

entry depths_s [n] (s: string [n]) : ([]i64, []i32) =
  let (depths, values) = unzip (depths (input.steps s))
  in (depths, values)

-- ==
-- entry: depths_s
-- input {"d0 d2 d3 u u d5 u"}
-- output {[0i64,1i64,2i64,1i64] [0i32,2i32,3i32,5i32]}

-- ==
-- entry:Eparents
-- input {[0i64, 1i64, 2i64, 1i64]}
-- output {[0i64, 0i64, 1i64, 0i64]}
-- input {[0i64, 1i64, 1i64, 1i64, 2i64]}
-- output {[0i64, 0i64, 0i64, 0i64, 3i64]}
-- input {[0i64, 1i64, 1i64, 1i64, 2i64, 2i64, 1i64, 2i64]}
-- output {[0i64, 0i64, 0i64, 0i64, 3i64, 3i64, 0i64, 6i64]}
-- input {[0i64, 1i64, 2i64, 3i64, 4i64]}
-- output {[0i64, 0i64, 1i64, 2i64, 3i64]}
-- input  {[0i64, 1i64, 1i64, 2i64, 3i64, 2i64, 1i64]}
-- output {[0i64, 0i64, 0i64, 2i64, 3i64, 2i64, 0i64]}
-- input  {[0i64, 1i64, 2i64, 3i64, 2i64, 1i64, 2i64]}
-- output {[0i64, 0i64, 1i64, 2i64, 1i64, 0i64, 5i64]}
-- input  {[0i64, 1i64, 2i64, 1i64, 2i64, 3i64]}
-- output {[0i64, 0i64, 1i64, 0i64, 3i64, 4i64]}
-- input  {[0i64, 1i64, 2i64, 3i64, 4i64, 5i64, 1i64, 2i64, 3i64, 4i64, 5i64]}
-- output {[0i64, 0i64, 1i64, 2i64, 3i64, 4i64, 0i64, 6i64, 7i64, 8i64, 9i64,]}

--[0,1,1,1,2,2,1,2]
-- ==
-- entry: Esubtree_sizes
-- input {"d1 d2 d3 d4 d5"}
-- output {[15i64, 14i64, 12i64, 9i64, 5i64]}
-- input {"d1 d2 d3 d4 d5 u u u u d2 d3 d4 d5"}
-- output {[29i64, 14i64, 12i64, 9i64, 5i64, 14i64, 12i64, 9i64, 5i64]}
-- input {"d0 d2 u d3 u d0 d4 u d0 u u d0 d5"}
-- output {[14i64, 2i64, 3i64, 4i64, 4i64, 0i64, 5i64, 5i64]}