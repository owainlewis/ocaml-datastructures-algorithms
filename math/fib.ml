(* Improving fibs *)

let naive_fib i =
  if i <= 1 then i
            else naive_fib(i-1) + naive_fib(i-2)

let rec n_fib i = naive_fib i

(* Dynamic programming variants *)

module IntMap = Map.Make(struct type t = int
                         let compare = compare end)

let time f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    (Sys.time() -. t)

let show_map_values m = IntMap.iter(fun k v-> print_endline (string_of_int v)) m
