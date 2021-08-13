open Utils

type t = {
      title: string;
      tags: Tag.t list;
      body: string;
}

let make title tags body = { title; tags; body } 
let empty = make "" [] ""

let get_title {title; _} = title
let get_tags {tags; _} = tags
let get_body {body; _} = body

let has_tag tag t =
      t.tags
      |> List.exists @@ (=) tag

let has_no_tag = get_tags >> (=) []

let eq a b = a.title = b.title

