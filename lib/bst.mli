exception Unknown
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
val leaf : 'a tree
val node : 'a -> 'a tree
val min_value : 'a tree -> 'a
val max_value : 'a tree -> 'a
val insert : 'a -> 'a tree -> 'a tree
val delete : 'a -> 'a tree -> 'a tree
val member : 'a -> 'a tree -> bool
val height : 'a tree -> int
val fold : ('a -> 'b -> 'b -> 'b) -> 'b -> 'a tree -> 'b
val build : 'a list -> 'a tree
