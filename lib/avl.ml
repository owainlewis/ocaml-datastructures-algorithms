module AvlTree =
  struct
    (* OCaml AVL Trees *)
    type 'a tree = Node of 'a * 'a tree * 'a tree | Leaf

    let rec insert x = function
        Leaf -> Node (x, Leaf, Leaf)
      | Node (y, l, r) as t ->
          if x = y then t
          else if x > y then Node (y, l, (insert x r))
          else               Node (y, (insert x l), r)

    let rec contains x = function
        Leaf -> false
      | Node (y, l, r) ->
          if x = y then true
                   else if (x < y) then contains x l
                   else contains x r

    let insert_many = List.fold_left (fun acc x -> insert x acc) Leaf

  end
