type t = {
      items: Item.t list;
      filters: Filter.t list;
      selected: Filter.t * Item.t;
}

let empty = {
      items=[];
      filters=[];
      selected=(Filter.empty, Item.empty);
} 

let make items filters = {
      items;
      filters;
      selected=(
            Filter.empty,
            try List.nth items 0 with Failure _ -> Item.empty
      ); 
}

let get_items { items; _ } filter =
      if filter = Filter.empty
      then Item.empty :: items
      else Item.empty :: Filter.filter filter items

let get_filters { filters; _ } = Filter.empty :: filters
let get_selected { selected; _ } = selected

let select_first t =
      let (filter, item) = t.selected in
      let filtered_items = get_items t filter in
      let first =
            match List.nth_opt filtered_items 0 with
            | Some item -> item
            | None -> item
      in
      let selected = (filter, first) in
      { t with selected }

let add_items items t =
      let items = items @ t.items in
      select_first { t with items }
let add_filters filters t =
      let filters = filters @ t.filters in
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

