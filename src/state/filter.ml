type rule =
    | WithTag of Tag.t Id.t

type t = {
    name: string;
    rules: rule list;
}

let empty = {
    name="";
    rules=[];
}

let make name rules = { name; rules }

let is_suitable rules item =
    let fold acc rule = 
        if acc = false then false else
        match rule with
        | WithTag tag_id -> Item.has_tag tag_id item
    in
    List.fold_left fold false rules

let filter t items = 
    List.filter (is_suitable t.rules) items

(* --- TEST --- *)

let%test "sample" = true

