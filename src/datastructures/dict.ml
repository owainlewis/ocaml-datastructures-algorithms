(* A dictionary is a mapping of string -> value of type alpha *)

module type DICTIONARY =
  sig
    type k = string
    type 'a dict
    val make : unit -> 'a dict
    val insert : 'a dict -> k -> 'a -> 'a dict
    val lookup : 'a dict -> k -> 'a
    exception NotFound
  end

(* An association list is a linked list where every element has a key and a value *)

module AssocList : DICTIONARY =
  struct
    type k = string

    (* The association list definition *)
    type 'a dict = (k * 'a) list

    let make(): 'a dict = []

    let insert(d: 'a dict) (key: k) (item: 'a) =
      (key, item) :: d

    exception NotFound
    let rec lookup (d : 'a dict) (k : k) : 'a =
      match d with
        [] -> raise NotFound
      | (k', x) :: rest -> if k = k' then x
                                     else lookup rest k
  end
