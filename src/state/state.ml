open Entities
open For_ui

module Parser = Parser
module DetailState = DetailState
module ViewState = ViewState

module DB = struct
      let connection_url = Printf.sprintf "sqlite3://%s/items.db" Sys.(getcwd ())

      module Q = struct
            let create_tmp = Caqti_request.exec Caqti_type.unit {| CREATE TABLE IF NOT EXISTS items (
                  id INTEGER PRIMARY KEY,
                  tags TEXT,
                  title TEXT,
                  body TEXT
               )
            |}
      end

      let run (module Db : Caqti_blocking.CONNECTION) =
            Db.exec Q.create_tmp () |> Caqti_blocking.or_fail

      let cnt = Caqti_blocking.connect (Uri.of_string connection_url)
            |> Caqti_blocking.or_fail
            |> run
end

type navigation_msg =
      | ToDetail of Item.t Id.t * Item.t list
      | ToView of Item.t list
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

let update state msg = match state, msg with
      | (View view_data, ViewMsg msg) -> View ViewState.(update view_data msg)
      | (Detail detail, DetailMsg msg) -> Detail DetailState.(update detail msg)
      | (_, NavigationMsg msg) -> begin match msg with
            | ToDetail (id, items) -> Detail DetailState.(update empty (Init (id, items)))
            | ToView updated_items ->
                  let file_items, file_filters = Parser.of_file "data.csv" in
                  let items =
                        file_items
                        |> List.map (fun file_item ->
                              (* always not found, becouse ids are randomly generated after reading from file*)
                              try List.find (Item.eq file_item) updated_items
                              with Not_found -> file_item
                        )
                  in
                  View {items; filters=file_filters; selected=Selected.empty; input=Input.empty}
            | Nothing -> state
            | Quit -> state
      end
      | _ -> state

