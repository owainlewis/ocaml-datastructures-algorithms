(* A dictionary is a mapping of string -> value of type alpha *)

module type DICTIONARY =
  sig
    type k = string
    type 'a dict
    val make    : unit -> 'a dict
    val insert  : 'a dict -> k -> 'a -> 'a dict
    val lookup  : 'a dict -> k -> 'a option
    val keys    : 'a dict -> string list
    val vals    : 'a dict -> 'a list
    val builder : (k * 'a) list -> 'a dict
    exception NotFound
  end

(* An association list is a linked list where every element has a key and a value *)

module E = struct
  type 'a t = Left of 'a | Right of exn
end

module AssocList : DICTIONARY =
  struct
    type k = string
    type 'a dict = (k * 'a) list

    let make(): 'a dict = []

    let insert(d: 'a dict) (key: k) (item: 'a) = (key, item) :: d

    exception NotFound

    let rec lookup(d: 'a dict) (k: string): 'a option =
      match d with
        [] -> None
      | (k',x)::rest -> if k = k' then Some(x)
                                  else lookup rest k

    let keys(d : 'a dict) =
      let rec aux(d, ks) =
        match d with
        | [] -> ks
        | (k,_)::xs -> k::aux(xs, ks)
      in aux(d, [])

    let vals(d: 'a dict): 'a list =
      let rec aux(d, vs) =
        match d with
        | [] -> vs
        | (_,v)::xs -> v::aux(xs, vs)
      in aux(d, [])

    let builder(l: (k * 'a) list) =
      let empty = make() in
      let rec aux(l, d) =
        match l with
        | [] -> d
        | (k,v)::xs -> aux(xs, (insert d k v))
      in aux(l, empty)
  end
