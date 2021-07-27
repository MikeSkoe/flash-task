type t = {
      id: t Id.t;
      title: string;
      tags: string list;
      body: string;
}

let make title tags body = 
      let id = Id.of_string title in
      { id; title; tags; body } 

let get_id {id; _} = id
let get_tags {tags; _} = tags
let get_title {title; _} = title
let get_body {body; _} = body

let has_tag (tag_id: Tag.t Id.t) (t: t) =
      let tag_ids = List.map Id.of_string t.tags in
      List.mem tag_id tag_ids

let compare a b = Id.compare a.id b.id


(* --- TEST --- *)

let sample_1 = make
      "this is the title"
      [Tag.(get_title sample_1); Tag.(get_title sample_2)]
      "this is the body of an item"

let sample_2 = make
      "this is the second item's title"
      [Tag.(get_title sample_2)]
      "this is the second body of a second item"

