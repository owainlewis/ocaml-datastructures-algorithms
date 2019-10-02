let rec bubble xs =
  let rec aux ys = match ys with
    | []    -> []
    | x::[] -> [x]
    | x::y::xs when x > y -> y::aux(x::xs)
    | x::y::xs -> x :: aux(y::xs)
  in
  let sweep = aux xs in
  if sweep = xs
    then sweep
  else bubble sweep

let rec insertion xs =
  let rec aux v ys =
    match ys with
    | [] -> [v]
    | z::zs as l ->
      if v < z
        then v :: l
        else z :: (aux v zs)
  in match xs with
  | [] -> []
  | [x] -> [x]
  | v::vs -> aux v (insertion vs)
  
                
