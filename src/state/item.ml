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

(* Checkers *)
let has_tag tag = get_tags >> List.exists @@ (=) tag

let has_no_tag = get_tags >> (=) []

let eq a b = a.id = b.id

