module Folder = Folder
module Filter = Filter
module Tag = Tag
module Item = Item
module Parser = Parser
module Selected = Selected

module ViewState = struct
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
end

module DetailState = struct
      module EditData = struct
            type t =
                  | NewItem of For_ui.Textarea.t
                  | ExistingItem of Item.t Id.t * For_ui.Textarea.t

            let map fn = function
                  | NewItem textarea -> NewItem (fn textarea)
                  | ExistingItem (id, textarea) -> ExistingItem (id, (fn textarea)) 
      end

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
end

type navigation_msg =
      | Save of Folder.t * Item.t
      | ToDetail of (Folder.t * DetailState.EditData.t)
      | ToView
      | Nothing
      | Quit

type msg =
      | ViewMsg of ViewState.msg
      | DetailMsg of DetailState.msg
      | NavigationMsg of navigation_msg

type t =
      | View of ViewState.t
      | Detail of DetailState.t

let empty = View Folder.empty

let unwrap_folder = function
      | View folder -> folder
      | Detail (folder, _) -> folder

let unwrap_selected = function
      | View folder -> Folder.get_selected folder
      | Detail (folder, _) -> Folder.get_selected folder

let update state msg = match state, msg with
      | (View folder, ViewMsg msg) ->
            View ViewState.(update folder msg)

      | (Detail (folder, edit_data), DetailMsg msg) ->
            Detail DetailState.(update folder edit_data msg)

      | (_, NavigationMsg msg) -> begin match msg with
            | Save (folder, item) -> View Folder.(add_items [item] folder)
            | ToDetail (folder, edit_data) -> Detail (folder, edit_data)
            | ToView -> View (unwrap_folder state)
            | Nothing -> state
            | Quit -> state
      end
      | _ -> state

