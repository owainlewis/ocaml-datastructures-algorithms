(* Bag ADT *)

module type BAG =
  sig
    type 'a list
    val add : 'a bag
    (* get *)
    (* empty *)
    (* full *)
    (* size *)
    (* capacity *)
  end

module Bag =
  struct
    type 'a bag = 'a list
    let add (item : 'a) (bag: 'a bag): 'a bag =
      item :: bag
    let empty(bag: 'a bag) = bag == []
  end

