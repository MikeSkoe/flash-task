module Folder = Folder
module Tag = Tag
module Item = Item

type msg =
      | Next
      | Prev
      | Nothing
      | Quit

type t = View of Folder.t * Filter.t

let empty = View (Folder.sample, Filter.empty)

let update (View (folder, filter)) = function
      (*
      | Next -> Folder.(t next folder)
      | Prev -> Folder.(t prev folder)
      *)
      | Next -> View (folder, filter)
      | Prev -> View (folder, filter)
      | Nothing -> View (folder, filter)
      | Quit -> View (folder, filter)

