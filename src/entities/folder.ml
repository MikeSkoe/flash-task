type t = {
      items: Item.t list;
      filters: Filter.t list;
      selected: Selected.t;
}

let empty = {
      items=[];
      filters=[];
      selected=Selected.empty;
} 

let make items filters = {
      empty with
      items;
      filters;
}

let get_items { items; _ } = items
let get_filters { filters; _ } = Filter.empty :: filters
let get_selected { selected; _ } = selected

let add_items items t =
      let old_items =
            t.items
            |> List.filter (fun item -> not (List.exists Item.(eq item) items))
      in
      let items = items @ old_items in
      { t with items }

let add_filters filters t =
      let filters = filters @ t.filters in
      { t with filters }

let delete_items items t =
      let items =
            t.items
            |> List.filter (fun item -> not (List.mem item items))
      in
      { t with items }

let delete_filters filters t =
      let filters =
            t.filters
            |> List.filter (fun filter -> not (List.mem filter filters))
      in
      { t with filters }

let shift_filter shift t =
      let selected = Selected.shift_filter (get_items t) (get_filters t) shift (get_selected t) in
      { t with selected }

let shift_item shift t = 
      let selected = Selected.shift_item (get_items t) shift (get_selected t) in
      { t with selected }

