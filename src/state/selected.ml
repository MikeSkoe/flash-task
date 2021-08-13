type t =
    | Item of Filter.t * Item.t;
    | Filter of Filter.t;
    | NewItem of Item.t;
    | NoSelected

let empty = NoSelected

let select_first (folder: Folder.t) t =
      let (filter, item) = t.selected in
      let filtered_items = get_items t filter in
      let first =
            match List.nth_opt filtered_items 0 with
            | Some item -> item
            | None -> item
      in
      let selected = (filter, first) in
      { t with selected }

