open Entities
open Utils
open For_ui

type t = {
      items: Item.t list;
      filters: Filter.t list;
      selected: Selected.t;
      input: Input.t;
}

let empty = {
      items=[];
      filters=[];
      selected=Selected.empty;
      input=Input.empty;
}

type msg = 
      | NextItem
      | PrevItem
      | NextFilter
      | PrevFilter
      | DeleteItem of Item.t
      | DeleteFilter of Filter.t
      | Input of Input.msg
      | AddItem of string

let shift_filter shift {items; filters; selected; input} =
      let selected =
            selected
            |> Selected.shift_filter filters shift
      in
      {items; filters; selected; input}

let shift_item shift {items; filters; selected; input} =
      let selected =
            selected
            |> Selected.shift_item items shift
      in
      {items; filters; selected; input}

(* TODO: accept module for eq?*)
let filter_out_element eq x = List.filter ((eq x) >> not)

let delete_item item {items; filters; selected; _} = 
      let items = filter_out_element Item.eq item items in
      let selected = Selected.normalize filters items selected in
      let input = Input.empty in
      {items; filters; selected; input}

let delete_filter filter {items; filters; selected; _} = 
      let filters = filter_out_element Filter.eq filter filters in
      let selected = Selected.normalize filters items selected in
      let input = Input.empty in
      {items; filters; selected; input}

let add_item title {items; filters; selected; _} =
      let item = Item.make title "" in
      let items = item :: items in
      let input = Input.empty in
      {items; filters; selected; input}

let change_input msg {items; filters; selected; input} = 
      let input = Input.update msg input in
      {items; filters; selected; input}

let update = function
      | DeleteItem item -> delete_item item
      | DeleteFilter filter -> delete_filter filter
      | AddItem title -> add_item title
      | NextFilter -> shift_filter 1
      | PrevFilter -> shift_filter (-1)
      | NextItem -> shift_item 1
      | PrevItem -> shift_item (-1)
      | Input msg -> change_input msg

