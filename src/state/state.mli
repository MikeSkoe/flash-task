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

val empty : t
val update : t -> msg -> t
val of_file : string -> t

