module Folder = Folder
module Tag = Tag
module Item = Item
module ItemSet = Folder.ItemSet
module TagSet = Folder.TagSet

let first_item = Item.make
      "the title of the first thing"
      ["first_tag"]
      "body of the\nthing btw"

let second_item = Item.make
      "the title of the second stuff"
      ["first_tag"; "second_tag"]
      "body of the\nthing btw"

type msg =
      | Next
      | Prev
      | Nothing
      | Quit

type t = View of Folder.t

let empty = View(
      Folder.empty
      |> Folder.add_item first_item
      |> Folder.add_item second_item
)

let update (View folder) = function
      | Next -> Folder.(t next folder)
      | Prev -> Folder.(t prev folder)
      | Nothing -> state
      | Quit -> state
