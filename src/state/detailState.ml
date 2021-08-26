open Entities
open For_ui

type t =
      | ItemEdit of (File.t * EditData.t)
      | FilterEdit of (File.t * EditData.t)

type msg =
      | NextItem
      | PrevItem
      | Input of Textarea.msg
      | SaveItem of Item.t
      | SaveFilter of Filter.t

let map fn = function
      | ItemEdit (folder, edit_data) -> ItemEdit (fn (folder, edit_data))
      | FilterEdit (folder, edit_data) -> FilterEdit (fn (folder, edit_data))

let shift_item shift (folder, edit_data) = match edit_data with
      | EditData.NewItem _ -> (folder, edit_data)
      | EditData.ExistingItem _ ->
            let folder = File.shift_item shift folder in
            let edit_data =
                  File.get_selected folder
                  |> Selected.get_item
                  |> EditData.of_item
            in
            (folder, edit_data)
      | EditData.NewFilter _ -> (folder, edit_data)
      | EditData.ExistingFilter _ -> (folder, edit_data)

let shift_cursor (shift_x, shift_y) (folder, edit_data) =
      let edit_data = 
            edit_data
            |> EditData.map Textarea.(shift_cursor (shift_x, shift_y))
      in
      (folder, edit_data)

let save_item item (folder, edit_data) = File.add_items [item] folder, edit_data
let save_filter filter (folder, edit_data) = File.add_filters [filter] folder, edit_data

let type_char chr (folder, edit_data) = folder, EditData.map Textarea.(type_char chr) edit_data

let del_char (folder, edit_data) = folder, EditData.map Textarea.del_char edit_data

let update t msg = match t, msg with
      | ItemEdit _ as t, NextItem -> map (shift_item 1) t
      | ItemEdit _ as t, PrevItem -> map (shift_item (-1)) t
      | ItemEdit _ as t, SaveItem item -> map (save_item item) t
      | ItemEdit _ as t, SaveFilter _ -> t

      | FilterEdit _ as t, NextItem -> t
      | FilterEdit _ as t, PrevItem -> t
      | FilterEdit _ as t, SaveItem _ -> t
      | FilterEdit _ as t, SaveFilter filter -> map (save_filter filter) t

      | t, Input msg -> map (fun (folder, edit_data) -> (folder, EditData.map Textarea.(update msg) edit_data)) t

