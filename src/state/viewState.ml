open Entities

type t = Folder.t

type msg = 
    | NextItem
    | PrevItem
    | NextFilter
    | PrevFilter
    | DeleteItem of Item.t
    | DeleteFilter of Filter.t

let update folder = function
    | DeleteItem item -> Folder.(delete_items [item] folder)
    | DeleteFilter filter -> Folder.(delete_filters [filter] folder)

    | NextFilter -> Folder.shift_filter 1 folder
    | PrevFilter -> Folder.shift_filter (-1) folder
    | NextItem -> Folder.shift_item 1 folder
    | PrevItem -> Folder.shift_item (-1) folder

