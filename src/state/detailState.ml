open Entities

type t = Folder.t * EditData.t

type msg =
    | NextItem
    | PrevItem
    | ShiftCursor of int * int
    | TypeChar of char
    | DelChar

let update folder edit_data = function
    | NextItem -> (folder, edit_data)
    | PrevItem -> (folder, edit_data)
    | ShiftCursor (shift_x, shift_y) ->
          let edit_data =
                edit_data
                |> EditData.map For_ui.Textarea.(shift_cursor (shift_x, shift_y))
          in
          (folder, edit_data)
    | TypeChar chr ->
          let edit_data =
                edit_data
                |> EditData.map For_ui.Textarea.(type_char chr)
          in
          (folder, edit_data)
    | DelChar ->
          let edit_data =
                edit_data
                |> EditData.map For_ui.Textarea.del_char 
          in
          (folder, edit_data)
