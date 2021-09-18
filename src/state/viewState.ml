open Entities
open Utils
open For_ui

type t = File.t * Input.t

let empty = (File.empty, Input.empty)

type msg = 
      | NextItem
      | PrevItem
      | NextFilter
      | PrevFilter
      | DeleteItem of Item.t
      | DeleteFilter of Filter.t
      | Input of Input.msg
      | AddItem of string

let update (file, input) = function
      | DeleteItem item -> (File.(delete_items [item] file), Input.empty)
      | DeleteFilter filter -> (File.(delete_filters [filter] file), Input.empty)
      | AddItem title ->
            let tags =
                  file
                  |> File.get_selected
                  |> Selected.get_filter
                  |> Filter.(get_rule >> tags_of)
            in
            let item = Item.make title tags "" in
            let file = File.add_items [item] file in
            (file, Input.empty)
      
      | NextFilter -> (File.shift_filter 1 file, input)
      | PrevFilter -> (File.shift_filter (-1) file, input)
      | NextItem -> (File.shift_item 1 file, input)
      | PrevItem -> (File.shift_item (-1) file, input)
      | Input msg -> (file, Input.update msg input)

