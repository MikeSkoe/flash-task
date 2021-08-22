open Entities

type t = File.t

type msg = 
    | NextItem
    | PrevItem
    | NextFilter
    | PrevFilter
    | DeleteItem of Item.t
    | DeleteFilter of Filter.t

let update folder = function
    | DeleteItem item -> File.(delete_items [item] folder)
    | DeleteFilter filter -> File.(delete_filters [filter] folder)

    | NextFilter -> File.shift_filter 1 folder
    | PrevFilter -> File.shift_filter (-1) folder
    | NextItem -> File.shift_item 1 folder
    | PrevItem -> File.shift_item (-1) folder

