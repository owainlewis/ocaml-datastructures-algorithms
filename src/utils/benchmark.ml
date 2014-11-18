module Benchmark = struct
  let time ~func:f ~value:x =
    let t = Sys.time() in
    let _ = f x in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    Sys.time() -. t
end

(* Time.time (fun x -> x * 1000) 10;; *)
(* Execution time: 0.000002s
  - : float = 2.00000000001e-05 *)
