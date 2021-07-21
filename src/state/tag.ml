type t = {
      id: Id.t;
      title: string;
}

let make title = 
      let id = Id.(Tag(of_string title)) in
      { id; title } 

let compare a b = Id.(compare a.id b.id)

