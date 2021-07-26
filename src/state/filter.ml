open Utils

type t = {
      tags: Tag.t Id.t list;
      items: Item.t Id.t list;
      selected: Item.t Id.t option;
}

let empty = {
      tags=[];
      items=[];
      selected=None;
}

let filter_items tags items =
      let some_tag_in_item = 
            Item.get_tags
            >> List.map Id.of_string
            >> Utils.intersect tags
      in
      let no_filter _ = true in
      items
      |> List.filter (if tags = [] then no_filter else some_tag_in_item)
      |> List.map Item.get_id

let get_first list =
      try Some (List.hd list)
      with Failure _ -> None

let get_selected {selected; _} = selected

let update_items all_items t =
      let items = filter_items t.tags all_items in
      let selected = get_first items in
      { t with items; selected }

let add_tag tag_id all_items t =
      if List.mem tag_id t.tags then t else
      let tags = tag_id :: t.tags in
      update_items all_items { t with tags}

let remove_tag tag_id all_items t =
      if not @@ List.mem tag_id t.tags then t else
      let tags = List.filter ((=) tag_id >> not) t.tags in
      update_items all_items { t with tags}

let next t =
      let selected =
            match t.selected with
            | Some selected ->
                  let rec iter = (function
                        | [] -> Some selected
                        | item_id :: next_id :: _tail when item_id = selected -> Some next_id
                        | _item_id :: tail -> iter tail
                  ) in
                  iter t.items
            | None -> None
      in
      { t with selected }

let prev t =
      let selected =
            match t.selected with
            | Some selected ->
                  let rec iter = (function
                        | [] -> Some selected
                        | prev_id :: item_id :: _tail when item_id = selected -> Some prev_id
                        | _item_id :: tail -> iter tail
                  ) in
                  iter t.items
            | None -> None
      in
      { t with selected }


(* -- TESTS -- *)

let%test "\n--- [FILTER] add tag to filter\n" =
      let items = [Item.sample_1; Item.sample_2] in
      let with_tag = add_tag Tag.(get_id sample_1) items empty in 
      let expected = {
            tags = [Tag.(get_id sample_1)];
            items = [Item.(get_id sample_1)];
            selected = Some (Item.(get_id sample_1));
      } in
      with_tag = expected

let%test "\n--- [FILTER] first item selected after adding a tag + prev and next fnctions shift selected" =
      let items = [Item.sample_1; Item.sample_2] in
      let with_tag = add_tag Tag.(get_id sample_2) items empty in

      let with_next = next with_tag in
      let with_next_next = next with_next in

      let with_prev = prev with_tag in
      let with_next_prev = prev with_next in
      let with_next_next_prev = prev with_next_next in

      get_selected with_tag = Some Item.(get_id sample_1)
      && get_selected with_next = Some Item.(get_id sample_2)
      && get_selected with_next_next = Some Item.(get_id sample_2)
      && get_selected with_prev = Some Item.(get_id sample_1)
      && get_selected with_next_prev = Some Item.(get_id sample_1)
      && get_selected with_next_next_prev = Some Item.(get_id sample_1)

let%test "\n--- [FILTER] remove tag to filter\n" =
      let items = [Item.sample_1; Item.sample_2] in
      let with_tag =
            empty
            |> add_tag Tag.(get_id sample_1) items
            |> remove_tag Tag.(get_id sample_1) items
      in
      let expected = {
            tags = [];
            items = [Item.(get_id sample_1); Item.(get_id sample_2)];
            selected = Some (Item.(get_id sample_1));
      } in
      with_tag = expected

