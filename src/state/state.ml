open Entities

module Parser = Parser
module DetailState = DetailState
module ViewState = ViewState

type navigation_msg =
      | Save of Folder.t * Item.t
      | ToDetail of Item.t option
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

let get_folder = function
      | View folder -> folder
      | Detail (folder, _) -> folder

let update state msg = match state, msg with
      | (View folder, ViewMsg msg) ->
            View ViewState.(update folder msg)

      | (Detail (folder, edit_data), DetailMsg msg) ->
            Detail DetailState.(update folder edit_data msg)

      | (_, NavigationMsg msg) -> begin match msg with
            | Save (folder, item) -> View Folder.(add_items [item] folder)
            | ToDetail some_item ->
                  let folder = get_folder state in
                  let edit_data = match some_item with
                        | Some item ->
                              item
                              |> EditData.of_item 
                        | None ->
                              get_folder state
                              |> Folder.get_selected
                              |> Selected.get_filter
                              |> EditData.of_filter 
                  in
                  Detail (folder, edit_data)
            | ToView -> View (get_folder state)
            | Nothing -> state
            | Quit -> state
      end
      | _ -> state

