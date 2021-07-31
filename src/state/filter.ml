type rule =
    | WithTag of Tag.t

type t = {
    name: string;
    rules: rule list;
}

let make name rules = { name; rules }
let empty = {
    name="";
    rules=[];
}

let get_name {name; _} = name

let is_suitable rules item =
    let fold acc rule = 
        if acc = true then true else
        match rule with
        | WithTag tag_id -> Item.has_tag tag_id item
    in
    List.fold_left fold false rules

let filter t items = 
    List.filter (is_suitable t.rules) items

