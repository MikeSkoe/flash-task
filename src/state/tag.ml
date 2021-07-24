type t = {
      id: t Id.t;
      title: string;
}

let make title = 
      let id = Id.of_string title in
      { id; title } 

let get_title {title; _} = title
let get_id {id; _} = id

let compare a b = Id.compare a.id b.id


(* --- TEST --- *)

let sample_1 = make "first tag"
let sample_2 = make "second tag"

