module Folder = Folder
module Filter = Filter
module Tag = Tag
module Item = Item

type msg =
      | Next
      | Prev
      | Nothing
      | Quit

type t = View of Folder.t

let empty = View Folder.empty

let update (View folder) = function
      | Next -> View Folder.(next_selected folder)
      | Prev -> View Folder.(prev_selected folder)
      | Nothing -> View folder
      | Quit -> View folder

let of_file filename =
      let items =
            Csv.load filename
            |> List.map Parser.item_of_strings
      in
      View Folder.(Folder.add_items items empty)

