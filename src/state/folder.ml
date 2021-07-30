type t = {
      items: Item.t list;
      filters: Filter.t list;
      selected: Item.t Id.t option;
}

let empty = {
      items=[];
      filters=[];
      selected=None;
} 

let make items filters =
    let selected = match items with
    | head :: _tail -> Some Item.(get_id head)
    | _ ->  None
    in
    {
        items;
        filters;
        selected; 
    }

let get_items { items; _ } = items
let get_filters { filters; _ } = filters
let get_selected { selected; _ } = selected

let add_items items t =
      let items = items @ t.items in
      { t with items }

let add_filters filters t =
      let filters = filters @ t.filters in
      { t with filters }

let next_selected t = t

let prev_selected t = t


(* --- TEST --- *)

let%test "--- [FOLDER] " = true

