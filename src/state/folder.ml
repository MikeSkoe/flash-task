type t = {
      items: Item.t list;
      filters: Filter.t list;
}

let empty = {
      items=[];
      filters=[];
} 

let make items filters = {
      items;
      filters;
}

let get_items { items; _ } filter =
      let items =
            if filter = Filter.empty
            then items
            else Filter.filter filter items
      in
      items

let get_filters { filters; _ } = Filter.empty :: filters

let add_items items t =
      let old_items =
            t.items
            |> List.filter (fun item -> not (List.exists Item.(eq item) items))
      in
      let items = items @ old_items in
      select_first { t with items }

let add_filters filters t =
      let filters = filters @ t.filters in
      select_first { t with filters }

let delete_items items t =
      let items =
            t.items
            |> List.filter (fun item -> not (List.mem item items))
      in
      select_first { t with items }

let delete_filters filters t =
      let filters =
            t.filters
            |> List.filter (fun filter -> not (List.mem filter filters))
      in
      select_first { t with filters }

let shift_selected take_left selected lst =
      let rec iter = function
            | left :: right :: tail ->
                  begin match left = selected, right = selected with
                  | (false, true) when take_left = true -> left
                  | (true, false) when take_left = false -> right
                  | _ -> iter @@ right :: tail
                  end
            | _head :: tail ->
                  iter tail
            | _ -> selected
      in
      iter lst 

let next_filter t =
      let (filter, item) = t.selected in
      let filters = get_filters t in
      let filter = shift_selected false filter filters in
      select_first { t with selected=(filter, item) }

let prev_filter t =
      let (filter, item) = t.selected in
      let filters = get_filters t in
      let filter = shift_selected true filter filters in
      select_first { t with selected=(filter, item) }

let next_item t =
      let (filter, item) = t.selected in
      let items = get_items t filter in
      let item = shift_selected false item items in
      { t with selected=(filter, item) }

let prev_item t =
      let (filter, item) = t.selected in
      let items = get_items t filter in
      let item = shift_selected true item items in
      { t with selected=(filter, item) }


(* --- TEST --- *)

let%test "--- [FOLDER] " = true

