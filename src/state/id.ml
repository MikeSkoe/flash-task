let (>>) f1 f2 arg = f2 (f1 arg)

let counter = ref(0)

let of_string =
      String.to_seq
      >> Seq.map Char.code 
      >> List.of_seq 
      >> List.fold_left (+) 0 

let get_next () =
      let next_id = counter.contents + 1 in
      counter.contents <- next_id;
      next_id

type t =
      | Tag of int
      | Item of int 

let to_int = function
      | Tag id -> id
      | Item id -> id

let compare a b = (to_int a) - (to_int b)
