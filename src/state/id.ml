type 'a t = int

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

let print t = print_endline @@ Printf.sprintf "%d" t

let compare a b = a - b

