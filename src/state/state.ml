module Folder = Folder
module Filter = Filter
module Tag = Tag
module Item = Item

type msg =
      | NextItem
      | PrevItem
      | NextFilter
      | PrevFilter
      | Nothing
      | Quit

type t = View of Folder.t

let empty = View Folder.empty

let update (View folder) = function
      | NextItem -> View Folder.(next_item folder)
      | PrevItem -> View Folder.(prev_item folder)
      | NextFilter -> View Folder.(next_filter folder)
      | PrevFilter -> View Folder.(prev_filter folder)
      | Nothing -> View folder
      | Quit -> View folder

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

