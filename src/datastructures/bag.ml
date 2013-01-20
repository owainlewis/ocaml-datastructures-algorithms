(* Bag ADT *)

module type BAG =
  sig
    type 'a list
    val add : 'a bag
  end

module Bag =
  struct
    type 'a bag = 'a list
    let add (item : 'a) (bag: 'a bag): 'a bag =
      item :: bag
  end

