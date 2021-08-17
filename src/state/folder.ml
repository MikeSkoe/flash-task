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
            else Filter.apply filter items
      in
      items

let get_filters { filters; _ } = Filter.empty :: filters

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


(* --- TEST --- *)

let%test "--- [FOLDER] " = true

