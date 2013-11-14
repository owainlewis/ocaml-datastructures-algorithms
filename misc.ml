(* Misc stuff *)

module type M = sig
    val fisher_yates_shuffle : 'a array -> 'a array
end

module Misc : M = 
  struct
  (* Knuth Fisher Yates Shuffle *)
  let fisher_yates_shuffle arr =
    let swap arr i j =
      let temp = arr.(i) in
      arr.(i) <- arr.(j); arr.(j) <- temp
    and len = Array.length arr in
    for i = (len-1) downto 1 do
      (* Random shuffle *)
      let r = Random.int (i+1) in
      swap arr i r;
    done;
    arr
  end

