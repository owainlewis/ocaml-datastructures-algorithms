(* Test helpers *)

let testing descr p x = 
  if (p x) <> true then
    let message = "Failed test: " ^ descr
    in failwith message
  else true
