type t = {
      items: Item.t list;
      filters: Filter.t list;
      selected: Selected.t;
}

let empty = {
      items=[];
      filters=[Filter.empty];
      selected=Selected.empty;
} 

let make items filters = {
      empty with
      items;
      filters;
}

let get_items { items; _ } = items
let get_filters { filters; _ } = filters
let get_selected { selected; _ } = selected

let normalize t = {
      t with
      selected=Selected.(normalize t.filters t.items t.selected)
}

let add_items items t =
      let old_items =
            t.items
            |> List.filter (fun item -> not (List.exists Item.(eq item) items))
      in
      let items = items @ old_items in
      { t with items }

let add_filters filters t =
      let old_filters =
            t.filters
            |> List.filter (fun filter -> not (List.exists Filter.(eq filter) filters))
      in
      let filters = filters @ old_filters in
      { t with filters }

let delete_items items t =
      let items =
            t.items
            |> List.filter (fun item -> not (List.mem item items))
      in
      normalize { t with items }

let delete_filters filters t =
      let filters =
            t.filters
            |> List.filter (fun filter -> not (List.mem filter filters))
      in
      normalize { t with filters }

let shift_filter shift t =
      let selected =
            get_selected t
            |> Selected.shift_filter (get_items t) (get_filters t) shift
      in
      normalize { t with selected }

let shift_item shift t = 
      let selected =
            get_selected t
            |> Selected.shift_item (get_items t) shift
      in
      normalize { t with selected }

