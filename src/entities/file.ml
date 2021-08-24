open Utils

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
let get_filters { filters; _ } = filters
let get_selected { selected; _ } = selected

let normalize t = {
      t with
      selected=Selected.(normalize t.filters t.items t.selected);
      filters=if List.mem Filter.empty t.filters then t.filters else Filter.empty :: t.filters;
}

module CollectionHelpers = struct
      let add eq new_items old_items =
            let update_items item =
                  try List.find (eq item) new_items
                  with Not_found -> item
            in
            let old_items = old_items |> List.map update_items in
            let new_items = new_items |> List.(filter (fun item -> not (exists (eq item) old_items))) in
            new_items @ old_items

      let remove new_items = List.(filter ((flip mem) new_items >> not))
end

let add_items new_items t = {
      t with
      items=CollectionHelpers.add Item.eq new_items t.items
} |> normalize

let add_filters new_filters t = {
      t with
      filters=CollectionHelpers.add Filter.eq new_filters t.filters
} |> normalize 

let delete_items new_items t = {
      t with
      items=CollectionHelpers.remove new_items t.items
} |> normalize

let delete_filters new_filters t = {
      t with
      filters=CollectionHelpers.remove new_filters t.filters
} |> normalize

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

