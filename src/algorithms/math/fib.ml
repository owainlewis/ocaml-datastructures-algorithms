(* Improving fibs *)

let rec naive_fib i = if i <= 1
                      then i
                      else naive_fib (i - 1) + naive_fib(i - 2)
