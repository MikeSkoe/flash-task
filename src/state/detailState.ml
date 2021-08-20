open Entities
open For_ui

type t = Folder.t * EditData.t

type msg =
    | NextItem
    | PrevItem
    | ShiftCursor of int * int
    | TypeChar of char
    | Save of Item.t
    | DelChar

let update folder edit_data = function
    | NextItem -> 
            let folder = Folder.shift_item 1 folder in
            let edit_data =
                Folder.get_selected folder
                |> Selected.get_item
                |> EditData.of_item
            in
            (folder, edit_data)
    | PrevItem ->
            let folder = Folder.shift_item (-1) folder in
            let edit_data =
                Folder.get_selected folder
                |> Selected.get_item
                |> EditData.of_item
            in
            (folder, edit_data)
    | Save item -> (Folder.add_items [item] folder, edit_data)
    | ShiftCursor (shift_x, shift_y) ->
          let edit_data =
                edit_data
                |> EditData.map Textarea.(shift_cursor (shift_x, shift_y))
          in
          (folder, edit_data)
    | TypeChar chr ->
          let edit_data =
                edit_data
                |> EditData.map Textarea.(type_char chr)
          in
          (folder, edit_data)
    | DelChar ->
          let edit_data =
                edit_data
                |> EditData.map Textarea.del_char 
          in
          (folder, edit_data)

