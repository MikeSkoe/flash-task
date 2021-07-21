type t = {
      id: Id.t;
      title: string;
      tags: string list;
      body: string;
}

  let make title tags body = 
        let id = Id.(Item (get_next ())) in
        { id; title; tags; body } 

let compare a b = Id.(compare a.id b.id)

