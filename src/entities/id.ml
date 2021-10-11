open Utils

type 'a t = int

let pure i = i

let of_string =
      String.to_seq
      >> Seq.map Char.code 
      >> Seq.fold_left (+) 0 

let get_next () = Random.int 999

let empty = 0

let print t = print_endline @@ Printf.sprintf "%d" t
