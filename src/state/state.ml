open Entities
open For_ui

module Parser = Parser
module DetailState = DetailState
module ViewState = ViewState

type navigation_msg =
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

let empty = View (File.empty, Input.empty)

let get_file = function
      | View (file, _) -> file
      | Detail (DetailState.ItemEdit (file, _)) -> file
      | Detail (DetailState.FilterEdit (file, _)) -> file

let update state msg = match state, msg with
      | (View view_data, ViewMsg msg) ->
            View ViewState.(update view_data msg)
      | (_, ViewMsg msg) ->
            let file = get_file state in
            let data = (file, Input.empty) in
            View ViewState.(update data msg)

      | (Detail detail, DetailMsg msg) ->
            Detail DetailState.(update detail msg)
      | (_, DetailMsg msg) ->
            let file = get_file state in
            let data = DetailState.(ItemEdit (file, EditData.empty)) in
            Detail DetailState.(update data msg)

      | (_, NavigationMsg msg) -> begin match msg with
            | ToView -> View ((get_file state), Input.empty)
            | Nothing -> state
            | Quit -> state
      end

