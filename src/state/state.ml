module Folder = Folder
module Filter = Filter
module Tag = Tag
module Item = Item

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
      | Nothing
      | Quit


type msg =
      | ViewMsg of view_msg
      | DetailMsg of detail_msg
      | NavigationMsg of navigation_msg

type t =
      | View of Folder.t
      | Detail of Folder.t

let empty = View Folder.empty

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
      | (_, NavigationMsg msg) -> begin match msg with
            | ToDetail -> begin match folder with
                  | View folder -> Detail folder
                  | Detail folder -> Detail folder
            end
            | ToView -> begin match folder with
                  | View folder -> View folder
                  | Detail folder -> View folder
            end
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

