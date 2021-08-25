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
    | TypeChar of char

let update folder input = function
    | DeleteItem item -> (File.(delete_items [item] folder), input)
    | DeleteFilter filter -> (File.(delete_filters [filter] folder), input)

    | NextFilter -> (File.shift_filter 1 folder, input)
    | PrevFilter -> (File.shift_filter (-1) folder, input)
    | NextItem -> (File.shift_item 1 folder, input)
    | PrevItem -> (File.shift_item (-1) folder, input)
    | TypeChar chr -> (folder, Input.type_char chr input)

