open Utils

type 'a t = int

let of_string =
      String.to_seq
      >> Seq.map Char.code 
      >> Seq.fold_left (+) 0 

let get_next () = Random.int 999

let get_same () = 1

let print t = print_endline @@ Printf.sprintf "%d" t

