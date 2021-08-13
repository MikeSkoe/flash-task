type rule_item =
    | WithTag of Tag.t

type rule = 
    | WithoutTags
    | All
    | OptTag of rule_item list

type t = {
    title: string;
    rule: rule;
}

let make title rule = { title; rule }
let empty = {
    title="all items";
    rule=All;
}

let get_title {title; _} = title
let get_rule {rule; _} = rule

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

let filter t items = List.filter (is_suitable t.rule) items

