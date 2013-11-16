type color = Red | Black

type 'a rbtree =
  | Leaf
  | Node of color * 'a * 'a rbtree * 'a rbtree

(*

       Bz            Bz           Bx            Bx
      /  \          / \          /  \          /  \
     Ry  d         Rx  d        a    Rz       a    Ry
    /  \          / \               /  \          /  \
  Rx   c         a   Ry            Ry   d        b    Rz
 /  \               /  \          / \                /  \
a    b             b    c        b   c              c    d

*)

(** Check if x is a member of this tree **)
let rec member x = function
    Leaf -> false
  | Node (_, value, left, right) ->
      if x == value then true
      else if x < value then member x left
      else member x right

let balance = function
    Black, z, Node (Red, y, Node (Red, x, a, b), c), d
  | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
  | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
  | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
      Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | a, b, c, d ->
      Node (a, b, c, d)

(** Item insertion **)
let insert x s =
  let rec ins = function
      Leaf -> Node (Red, x, Leaf, Leaf)
    | Node (color, y, a, b) as s ->
        if x < y then balance (color, y, ins a, b)
        else if x > y then balance (color, y, a, ins b)
        else s
  in
    match ins s with
        Node (_, y, a, b) ->
          Node (Black, y, a, b)
      | Leaf -> (* guaranteed to be nonempty *)
          raise (Failure "RBT insert failed with ins returning leaf")
