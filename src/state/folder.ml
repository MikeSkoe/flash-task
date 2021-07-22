module TagSet = Set.Make(Tag) 
module ItemSet = Set.Make(Item)

type t = {
      itemSet: ItemSet.t;
      tagSet: TagSet.t;
      selected: Id.t option;
}

let empty = {
      itemSet=ItemSet.empty;
      tagSet=TagSet.empty;
      selected=None;
} 

let add_item (item: Item.t) t =
      let tags =
            item.tags
            |> List.map Tag.make
            |> List.fold_left (fun acc cur -> TagSet.add cur acc) TagSet.empty
      in
      let itemSet = ItemSet.add item t.itemSet in
      let tagSet = TagSet.union t.tagSet tags in
      let selected = Some item.id in
      { itemSet; tagSet; selected }

