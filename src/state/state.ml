module Folder = Folder
module Filter = Filter
module Tag = Tag
module Item = Item
module Parser = Parser

type view_msg =
      | NextItem
      | PrevItem
      | NextFilter
      | PrevFilter

type detail_msg =
      | NextItem
      | PrevItem

type navigation_msg =
      | ToDetail
      | ToView
      | ToAdd
      | Nothing
      | Quit

type add_msg =
      | Save of Item.t
      | Cancel

type msg =
      | ViewMsg of view_msg
      | DetailMsg of detail_msg
      | NavigationMsg of navigation_msg
      | AddMsg of add_msg

type t =
      | View of Folder.t
      | Detail of Folder.t
      | Add of Folder.t

let empty = View Folder.empty

let unwrap_folder = function
      | View folder -> folder
      | Detail folder -> folder
      | Add folder -> folder

let update folder msg = match folder, msg with
      | (View folder, ViewMsg msg) -> begin match msg with
            | NextItem -> View Folder.(next_item folder)
            | PrevItem -> View Folder.(prev_item folder)
            | NextFilter -> View Folder.(next_filter folder)
            | PrevFilter -> View Folder.(prev_filter folder)
      end
      | (Detail folder, DetailMsg msg) -> begin match msg with
            | NextItem -> Detail Folder.(next_item folder)
            | PrevItem -> Detail Folder.(prev_item folder)
      end
      | (Add folder, AddMsg msg) -> begin match msg with
            | Save item -> View Folder.(add_items [item] folder)
            | Cancel -> View folder
      end
      | (_, NavigationMsg msg) -> begin match msg with
            | ToDetail -> Detail (unwrap_folder folder)
            | ToView -> View (unwrap_folder folder)
            | ToAdd -> Add (unwrap_folder folder)
            | Nothing -> folder
            | Quit -> folder
      end
      | _ -> folder

let of_file filename =
      let items =
            Csv.load filename
            |> List.map Parser.item_of_strings
      in
      View Folder.(
            add_items items empty
            |> add_filters @@ [
                  Filter.(make "---filter #tag" [WithTag Tag.(make "tag")]);
                  Filter.(make "---filter #tag3" [WithTag Tag.(make "tag3")]);
            ]
      )

let to_file filename state =
      Filter.empty
      |> Folder.get_items @@ unwrap_folder state
      |> List.map Parser.strings_of_item
      |> Csv.save filename

