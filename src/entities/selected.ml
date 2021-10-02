open Utils 

type t =
      | Item of Filter.t * Item.t
      | Filter of Filter.t

let empty = Filter Filter.empty

let get_filter = function 
      | Item (filter, _) -> filter
      | Filter filter -> filter

let get_item = function
      | Item (_, item) -> item
      | Filter _ -> Item.empty

let normalize filters items = function
      | Item (filter, item) ->
            begin match List.mem item items, List.mem filter filters with
            | (true, true) -> Item (filter, item)
            | (true, false) -> Item (Filter.empty, item)
            | (false, true) -> Filter filter
            | (false, false) -> Filter Filter.empty
            end
      | Filter filter ->
            if List.mem filter filters
            then Filter filter
            else Filter Filter.empty

let shift_filter filters shift t =
      try
            let filter = match t with
                  | Item (filter, _) -> filter
                  | Filter filter -> filter
            in
            let len = List.length filters in
            let filter_index = find ~eq:Filter.eq filter filters in
            let filter_index = max 0 @@ min (len - 1) (filter_index + shift) in
            let filter = List.nth filters filter_index in

            Filter filter
      with
            Failure _ -> t

let shift_item items shift t =
      try
            let filter = get_filter t in
            let item = get_item t in
            let items = Filter.apply filter items in
            let len = List.length items in
            let item_index =
                  let index = Item.(if item = empty then -1 else find ~eq item items) in
                  max (-1) @@ min (len - 1) (index + shift)
            in
            let item = List.nth items item_index in
            Item (filter, item)
      with
            | Invalid_argument _ -> Filter (get_filter t)
            | Failure _ -> t

