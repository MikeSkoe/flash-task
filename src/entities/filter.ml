type rule_item =
    | WithTag of Tag.t

type rule = 
    | WithoutTags
    | All
    | OptTag of rule_item list

type t = {
      id : t Id.t;
      title: string;
      rule: rule;
}

let make title rule = { title; rule; id=Id.get_next() }
let empty = {
    title="all items";
    rule=All;
    id=Id.get_same();
}

let get_title {title; _} = title
let get_rule {rule; _} = rule
let get_id {id; _} = id

let set_id id t = {t with id}

let return_true _ = true

let is_suitable = function
    | All -> return_true
    | WithoutTags -> Item.has_no_tag
    | OptTag rule_items -> 
        let fold item check (WithTag tag_id) = 
            if check = true then true else
            Item.has_tag tag_id item
        in
        (fun item -> List.fold_left (fold item) false rule_items)

let apply t items = List.filter (is_suitable t.rule) items

let eq = (=)

let tags_of = function
      | WithoutTags -> []
      | All -> []
      | OptTag rule_items ->
            rule_items
            |> List.map (function WithTag tag -> tag) 

