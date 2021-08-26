open Entities
open For_ui

type t = File.t * Input.t

type msg = 
      | NextItem
      | PrevItem
      | NextFilter
      | PrevFilter
      | DeleteItem of Item.t
      | DeleteFilter of Filter.t
      | Input of Input.msg
      | ApplyAction

let update folder input = function
      | DeleteItem item -> (File.(delete_items [item] folder), input)
      | DeleteFilter filter -> (File.(delete_filters [filter] folder), input)
      
      | NextFilter -> (File.shift_filter 1 folder, input)
      | PrevFilter -> (File.shift_filter (-1) folder, input)
      | NextItem -> (File.shift_item 1 folder, input)
      | PrevItem -> (File.shift_item (-1) folder, input)
      | Input msg -> (folder, Input.update msg input)
      | ApplyAction ->
              let folder = match String.trim input.text, File.get_selected folder with
              | ":delete", Selected.(Item (_, selected_item)) -> File.delete_items [selected_item] folder
              | _ -> folder
              in
              let input = Input.empty in
              (folder, input)

