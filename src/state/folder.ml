module TagSet = Set.Make(Tag) 
module ItemSet = Set.Make(Item)

type t = {
      itemSet: ItemSet.t;
      tagSet: TagSet.t;
      filter: Filter.t;
}

let empty = {
      itemSet=ItemSet.empty;
      tagSet=TagSet.empty;
      filter=Filter.empty;
} 

let get_items t = ItemSet.elements t.itemSet

let get_filter {filter; _} = filter

let add_item t (item: Item.t) =
      let tagSet =
            item
            |> Item.get_tags
            |> List.map Tag.make
            |> List.fold_left (fun acc tag -> TagSet.add tag acc) t.tagSet
      in
      let itemSet = ItemSet.add item t.itemSet in
      let filter = Filter.update_items ItemSet.(elements itemSet) t.filter in
      { itemSet; tagSet; filter }

let next_selected t =
      let filter = Filter.next t.filter in
      { t with filter }

let prev_selected t =
      let filter = Filter.prev t.filter in
      { t with filter }


(* --- TEST --- *)

let sample = List.fold_left add_item empty Item.[sample_1;sample_2]

let%test "--- [FOLDER] get items" =
      let items_ids =
            sample
            |> get_items 
            |> List.map Item.get_id
      in
      items_ids = [Item.(get_id sample_1); Item.(get_id sample_2)]

