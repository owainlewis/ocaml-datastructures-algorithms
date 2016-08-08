(* Search *)

module type SEARCHALG = sig
  val linear : 'a -> 'a list -> 'a option
  val binary : 'a -> 'a list -> 'a option
end

module Search = struct

  (* linear ~element:4 [1;2;3];; *)
  let rec linear ~element:v = function
    | [] -> None
    | x::xs -> if x = v then Some(x) else linear v xs

  let binary ~element:v xs =
    let rec aux element xs min max =
      (* Base clause *)
      if min = max
        then None
        else (* Inductive *)
          let t   = (min + max) in
          let mid = t / 2
          in Some(t)
    in match xs with
      | [] -> None
      | x::xs as lst ->
          aux v lst 0 (List.length xs)
end
