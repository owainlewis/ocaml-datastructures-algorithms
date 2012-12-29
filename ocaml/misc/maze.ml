#!/usr/bin/env ocaml

(* Maze algorithms *)

(* OCaml Conversion of program from 
   http://www.laurentluce.com/posts/solving-mazes-using-python-simple-recursivity-and-a-search/ *)

type choice = 
    Empty 
  | Unreachable
  | Ending 
  | Visited

let check v =
  match v with
  | 0 -> Empty
  | 1 -> Unreachable (* E.g a maze wall *)
  | 2 -> Ending
  | 3 -> Visited
  | _ -> failwith "No such type"

(* TODO OCaml matrix stuff is confusing as hell *)
let maze: ('int array array) =   
  [|[| 0, 0, 0, 0, 0, 1 |];
    [| 1, 1, 0, 0, 0, 1 |];
    [| 0, 0, 0, 1, 0, 0 |];
    [| 0, 1, 1, 0, 0, 1 |];
    [| 0, 1, 0, 0, 1, 0 |];
    [| 0, 1, 0, 0, 0, 2 |]|]

let search maze x y = 
  let row = maze.(x) in
    row
    