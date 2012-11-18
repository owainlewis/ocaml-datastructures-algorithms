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
           
(*  
def search(x, y):
    if grid[x][y] == 2:
        print 'found at %d,%d' % (x, y)
        return True
    elif grid[x][y] == 1:
        print 'wall at %d,%d' % (x, y)
        return False
    elif grid[x][y] == 3:
        print 'visited at %d,%d' % (x, y)
        return False
    
    print 'visiting %d,%d' % (x, y)

    # mark as visited
    grid[x][y] = 3

    # explore neighbors clockwise starting by the one on the right
    if ((x < len(grid)-1 and search(x+1, y))
        or (y > 0 and search(x, y-1))
        or (x > 0 and search(x-1, y))
        or (y < len(grid)-1 and search(x, y+1))):
        return True

    return False

search(0, 0) *)
