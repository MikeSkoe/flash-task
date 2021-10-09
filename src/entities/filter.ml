open Utils

type rule = 
    (* TODO: empty list instead of All case? *)
    | All
    | WithTags of string list

type t = {
      id: t Id.t;
      title: string;
      rule: rule;
}

let make title rule = { title; rule; id=Id.get_next() }

let empty = {
    title="all items";
    rule=All;
    id=Id.empty;
}

module Get = struct
      let title {title; _} = title
      let rule {rule; _} = rule
      let id {id; _} = id
end

module Set = struct
      let id id t = {t with id}
end

let return_true _ = true

let is_suitable = function
    | All -> return_true
    | WithTags tags -> 
        (fun item -> List.exists Item.((flip has_tag) item) tags)

let apply t items = List.filter (is_suitable t.rule) items

let eq a b = a.id = b.id

let tags_of = function
      | All -> []
      | WithTags tags -> tags

