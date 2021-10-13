type t = {
      id: t Id.t;
      title: string;
      tags: Tag.t list;
      body: string;
}

module Set = struct
      let id id t = {t with id}
end

let make ?id:(id=Id.get_next ()) ~title ~body () =
      let tags = Tag.tags_of_string title in
      { title; tags; body; id } 

let empty = make ~id:1 ~title:"" ~body:"" ()

(* Checkers *)
let has_tag tag t = List.exists ((=) tag) t.tags

let has_no_tag t = (=) [] t.tags

let eq a b = a.id = b.id

