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

val empty : t
val update : t -> msg -> t
val of_file : string -> t
val to_file : string -> t -> unit

