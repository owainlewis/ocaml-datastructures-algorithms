(* Red Black Trees *)

(* ******************************************************
 *
 *         Bz            Bz           Bx            Bx
 *        /  \          / \          /  \          /  \
 *       Ry  d         Rx  d        a    Rz       a    Ry
 *      /  \          / \               /  \          /  \
 *    Rx   c         a   Ry            Ry   d        b    Rz
 *    /  \               /  \          / \                /  \
 *    a    b             b    c        b   c              c    d
 *
 * ****************************************************************** *)

exception Insert of string

type color = Red | Black

type 'a tree =
  | Leaf
  | Node of color * 'a * 'a tree * 'a tree

let rec member x tree =
  match tree with
  | Leaf -> false
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
    raise (Insert "Red Black Tree insertion failed with `ins` returning leaf")
