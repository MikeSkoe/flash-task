type t =
    | Item of Filter.t * Item.t
    | Filter of Filter.t

let empty = Filter Filter.empty

let rec find x lst =
    match lst with
    | [] -> raise (Failure "Not Found")
    | h :: t -> if x = h then 0 else 1 + find x t

let shift_filter (folder: Folder.t) shift t =
      try
            let filter = match t with
                  | Item (filter, _) -> filter
                  | Filter filter -> filter
            in
            let filters = Folder.get_filters folder in
            let len = List.length filters in
            let filter_index = find filter filters in
            let filter_index = max 0 @@ min (len - 1) (filter_index + shift) in
            let filter = List.nth filters filter_index in

            begin match Folder.get_items folder filter with
            | [] -> Filter filter
            | first_item :: _tail -> Item (filter, first_item)
            end
      with
            Failure _ -> t

let shift_item (folder: Folder.t) shift t =
      try
            let filter = match t with
                  | Item (filter, _) -> filter
                  | Filter filter -> filter
            in
            let item = match t with
                  | Item (_, item) -> item
                  | Filter _ -> Item.empty
            in
            let items = Folder.get_items folder filter in
            let len = List.length items in
            let item_index =
                  if item = Item.empty
                  then -1
                  else find item items
            in
            let item_index = max 0 @@ min (len - 1) (item_index + shift) in
            let item = List.nth items item_index in
            Item (filter, item)
      with
            Failure _ -> t

