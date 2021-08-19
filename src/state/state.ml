open Entities

module Parser = Parser

type navigation_msg =
      | Save of Folder.t * Item.t
      | ToDetail of (Folder.t * EditData.t)
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

