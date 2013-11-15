(* Search *)

open Utils

module type SEARCHALG = sig
  val linear : 'a -> 'a list -> 'a option
end

module Search : SEARCHALG = struct
  (* Nice easy one to start with *)
  let rec linear v = function
    | [] -> None
    | x::xs -> if x = v then Some(x) else linear v xs
end
