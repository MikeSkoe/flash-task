open Utils

type t = {
      id: t Id.t;
      title: string;
      tags: Tag.t list;
      body: string;
}

let make title tags body = { title; tags; body; id=Id.get_next() } 
let empty = make "" [] ""

(* Getters *)
let get_title {title; _} = title
let get_tags {tags; _} = tags
let get_body {body; _} = body
let get_id {id; _} = id

(* Setters *)
let set_id id t = {t with id}
let set_tags tags t = {t with tags}

(* Checkers *)
let has_tag tag = get_tags >> List.exists @@ (=) tag

let has_no_tag = get_tags >> (=) []

let eq a b = a.id = b.id
