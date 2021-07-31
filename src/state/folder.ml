type t = {
      items: Item.t list;
      filters: Filter.t list;
      selected: Item.t option;
}

let empty = {
      items=[];
      filters=[];
      selected=None;
} 

let make items filters = {
      items;
      filters;
      selected=List.nth_opt items 0; 
}

let get_items { items; _ } filter =
      if filter = Filter.empty
      then items
      else Filter.filter filter items
let get_filters { filters; _ } = filters
let get_selected { selected; _ } = selected

let add_items items t =
      let items = items @ t.items in
      let selected = List.nth_opt items 0 in
      { t with items; selected }
let add_filters filters t =
      let filters = filters @ t.filters in
      { t with filters }

let sibling_selected take_left t =
      let rec iter = function
            | (left :: right :: tail, Some selected) ->
                  begin match Item.(equal left selected) , Item.(equal right selected) with
                  | (false, true) when take_left = true -> Some left
                  | (true, false) when take_left = false -> Some right
                  | _ -> iter (right :: tail, Some selected)
                  end
            | (_head :: tail, Some selected) ->
                  iter (tail, Some selected)
            | _ -> t.selected
      in
      let selected = iter (t.items, t.selected) in
      { t with selected }

let next_selected = sibling_selected false
let prev_selected = sibling_selected true


(* --- TEST --- *)

let%test "--- [FOLDER] " = true

