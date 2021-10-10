open Utils

type t = {
      id: t Id.t;
      title: string;
      tags: Tag.t list;
      body: string;
}

module Get = struct
      let title {title; _} = title
      let tags {tags; _} = tags
      let body {body; _} = body
      let id {id; _} = id
end

module Set = struct
      let id id t = {t with id}
      let tags tags t = {t with tags}
end

let make ?id:(id=Id.get_next()) title body =
      let tags = Tag.tags_of_string title in
      { title; tags; body; id } 

let empty = make "" ""

(* Checkers *)
let has_tag tag = Get.tags >> List.exists @@ (=) tag

let has_no_tag = Get.tags >> (=) []

let eq a b = a.id = b.id
