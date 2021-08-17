module Folder = Folder
module Filter = Filter
module Tag = Tag
module Item = Item
module Parser = Parser
module Selected = Selected

module ViewState = struct
      type t = Folder.t * Selected.t

      type msg = 
            | NextItem
            | PrevItem
            | NextFilter
            | PrevFilter
            | DeleteItem of Item.t
            | DeleteFilter of Filter.t

      let update (folder, selected) = function
            | DeleteItem item -> (Folder.(delete_items [item] folder), selected)
            | DeleteFilter filter -> (Folder.(delete_filters [filter] folder), selected)
            | NextFilter -> (folder, Selected.shift_filter folder 1 selected)
            | PrevFilter -> (folder, Selected.shift_filter folder (-1) selected)
            | NextItem -> (folder, Selected.shift_item folder 1 selected)
            | PrevItem -> (folder, Selected.shift_item folder (-1) selected)
end

module DetailState = struct
      type t = Folder.t * For_ui.Textarea.t

      type msg =
            | NextItem
            | PrevItem
            | ShiftCursor of int * int
            | TypeChar of char
            | DelChar

      let update (folder, edit_data) = function
            | NextItem -> (folder, edit_data)
            | PrevItem -> (folder, edit_data)
            | ShiftCursor (shift_x, shift_y) ->
                  let edit_data = For_ui.Textarea.(shift_cursor (shift_x, shift_y) edit_data) in
                  (folder, edit_data)
            | TypeChar chr ->
                  let edit_data = For_ui.Textarea.type_char chr edit_data in
                  (folder, edit_data)
            | DelChar ->
                  let edit_data = For_ui.Textarea.del_char edit_data in
                  (folder, edit_data)
end

type navigation_msg =
      | Save of Folder.t * Item.t
      | ToDetail of Item.t
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

let empty = View (Folder.empty, Selected.empty)

let unwrap_folder = function
      | View (folder, _) -> folder
      | Detail (folder, _) -> folder

let update state msg = match state, msg with
      | (View (folder, filter), ViewMsg msg) ->
            View ViewState.(update (folder, filter) msg)

      | (Detail (folder, edit_data), DetailMsg msg) ->
            Detail DetailState.(update (folder, edit_data) msg)

      | (_, NavigationMsg msg) -> begin match msg with
            | Save (folder, item) -> View (Folder.(add_items [item] folder), Selected.empty)
            | ToDetail item ->
                  let folder = unwrap_folder state in
                  let edit_data = For_ui.Textarea.make @@ Parser.string_of_item item in
                  Detail (folder, edit_data)
            | ToView -> View (unwrap_folder state, Selected.empty)
            | Nothing -> state
            | Quit -> state
      end
      | _ -> state

let of_file filename =
      let items =
            Csv.load filename
            |> List.map Parser.item_of_strings
      in
      View (
            Folder.(add_items items empty
                  |> add_filters @@ [
                        Filter.(make "---filter #tag" (OptTag [WithTag Tag.(make "tag")]));
                        Filter.(make "---filter #tag3" (OptTag [WithTag Tag.(make "tag3")]));
                  ])
            , Selected.empty
      )

let debug = Detail Folder.(empty, For_ui.Textarea.empty)

let to_file filename state =
      Filter.empty
      |> Folder.get_items @@ unwrap_folder state
      |> List.map Parser.strings_of_item
      |> Csv.save filename

