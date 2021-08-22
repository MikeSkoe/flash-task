open Entities

module Parser = Parser
module DetailState = DetailState
module ViewState = ViewState

type navigation_msg =
      | ToItemDetail of Item.t option
      | ToFilterDetail of Filter.t option
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
      | Detail (DetailState.ItemEdit (folder, _)) -> folder
      | Detail (DetailState.FilterEdit (folder, _)) -> folder

let update state msg = match state, msg with
      | (View folder, ViewMsg msg) ->
            View ViewState.(update folder msg)

      | (Detail detail, DetailMsg msg) ->
            Detail DetailState.(update detail msg)

      | (_, NavigationMsg msg) -> begin match msg with
            | ToItemDetail some_item ->
                  let folder = get_folder state in
                  let edit_data = match some_item with
                        | Some item ->
                              item
                              |> EditData.of_item 
                        | None ->
                              get_folder state
                              |> Folder.get_selected
                              |> Selected.get_filter
                              |> Filter.get_rule 
                              |> EditData.of_rule
                  in
                  Detail DetailState.(ItemEdit (folder, edit_data))
            | ToFilterDetail some_filter ->
                  let folder = get_folder state in
                  let edit_data = match some_filter with
                        | Some filter -> EditData.of_filter filter
                        | None -> EditData.of_filter Filter.empty
                  in
                  Detail DetailState.(FilterEdit (folder, edit_data))
            | ToView -> View (get_folder state)
            | Nothing -> state
            | Quit -> state
      end
      | _ -> state

