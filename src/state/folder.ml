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
      let tags =
            item.tags
            |> List.map Tag.make
            |> List.fold_left (fun acc cur -> TagSet.add cur acc) TagSet.empty
      in
      { 
            itemSet=ItemSet.(add item t.itemSet); 
            tagSet=TagSet.(union t.tagSet tags);
      }
