open Entities

module Parser = Parser
module DetailState = DetailState
module ViewState = ViewState

type navigation_msg =
      | ToDetail of Item.t Id.t
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

let empty = View ViewState.empty

module Make (API: Api_type.T) = struct
      let _ = API.ItemApi.create_table ()
      let _ = API.FilterApi.create_table ()

      module ViewState' = ViewState.Make (API)
      module DetailState' = DetailState.Make (API)

      let update state msg = match state, msg with
            | (View view_data, ViewMsg msg) -> View ViewState'.(update msg view_data)
            | (Detail detail, DetailMsg msg) -> Detail DetailState'.(update msg detail)
            | (_, NavigationMsg msg) -> begin match msg with
                  | ToDetail id -> Detail DetailState'.(update (Init id) DetailState.empty)
                  | ToView -> View ViewState'.(update Init ViewState.empty)
                  | Nothing -> state
                  | Quit -> state
            end
            | _ -> state
end

