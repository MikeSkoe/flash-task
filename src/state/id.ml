open Utils

type 'a t = int

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

let get_same () =
      1

let print t = print_endline @@ Printf.sprintf "%d" t

