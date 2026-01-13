(* Functional Queue Implementation *)

exception Empty_queue

type 'a t = {
  front: 'a list;
  back: 'a list;
}

let empty = { front = []; back = [] }

let is_empty q = q.front = [] && q.back = []

(* Normalize: move back to front when front is empty *)
let normalize q =
  match q.front with
  | [] -> { front = List.rev q.back; back = [] }
  | _ -> q

let enqueue x q =
  normalize { q with back = x :: q.back }

let dequeue q =
  let q = normalize q in
  match q.front with
  | [] -> raise Empty_queue
  | _ :: xs -> { q with front = xs }

let front q =
  let q = normalize q in
  match q.front with
  | [] -> raise Empty_queue
  | x :: _ -> x

let peek = front

let of_list lst =
  { front = lst; back = [] }

let to_list q =
  q.front @ List.rev q.back

let length q =
  List.length q.front + List.length q.back
