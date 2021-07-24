module TagSet = Set.Make(Tag) 
module ItemSet = Set.Make(Item)

type t = {
      itemSet: ItemSet.t;
      tagSet: TagSet.t;
}

let empty = {
      itemSet=ItemSet.empty;
      tagSet=TagSet.empty;
} 

let add_item (item: Item.t) t =
      let tagSet =
            item
            |> Item.get_tags
            |> List.map Tag.make
            |> List.fold_left (fun acc tag -> TagSet.add tag acc) t.tagSet
      in
      let itemSet = ItemSet.add item t.itemSet in
      { itemSet; tagSet }

let get_items t = ItemSet.elements t.itemSet

(* --- TEST --- *)

let sample = empty
      |> add_item Item.sample_1
      |> add_item Item.sample_2

let%test _ = true

