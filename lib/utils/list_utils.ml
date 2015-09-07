(* General list utils *)

module ListUtils = struct
  let safe_head = function
    | []    -> None
    | x::xs -> Some(x)

  let repeat x n =
    let rec aux curr bound result =
      if curr < bound then
        let next_generation = List.hd result :: result
        in aux (curr+1) bound next_generation
      else result
    in aux 1 n [x]

  let rec replicate n xs =
    let (|>) v f = f v in
    let rec aux n = function
      | [] -> []
      | x::xs -> let y = repeat x n
                 in y :: replicate n xs
    in
      let result = aux n xs in
      result |> List.fold_left (fun a b -> b :: a) []
end
