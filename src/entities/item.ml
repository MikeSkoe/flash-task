open Utils

type t = {
      id: t Id.t;
      title: string;
      tags: string list;
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

let make title body =
      let tags =
            String.split_on_char ' ' title
            |> List.map (String.to_seq >> List.of_seq)
            |> List.filter (function
                  | '#'::_ -> true
                  | _ -> false
            )
            |> List.map (List.to_seq >> String.of_seq)
      in
      { title; tags; body; id=Id.get_next() } 

let empty = make "" ""

(* Checkers *)
let has_tag tag = Get.tags >> List.exists @@ (=) tag

let has_no_tag = Get.tags >> (=) []

let eq a b = a.id = b.id

