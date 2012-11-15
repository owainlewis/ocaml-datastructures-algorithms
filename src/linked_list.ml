(* Single linked list *)

type 'a node = Nil | List of 'a * 'a node

type mutable_node = 
  { mutable next : mutable_node; 
    mutable prev : mutable_node;
    some_data : int } 

