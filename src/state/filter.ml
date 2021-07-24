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

let add_tag (tag_id: Tag.t Id.t) (folder: Folder.t) (t: t) : t =
      if List.mem tag_id t.tags then t else
      let items = folder
            |> Folder.get_items
            |> List.filter @@ Item.has_tag tag_id
            |> List.map Item.get_id
      in
      let tags = tag_id :: t.tags in
      let selected =
            try Some (List.hd items)
            with Failure _ -> None
      in
      { items; tags; selected }


(* -- TESTS -- *)

let%test "\n--- [FILTER] add tag to filter\n" =
      let with_tag = add_tag Tag.(get_id sample_1) Folder.sample empty in 
      let expected = {
            tags = [Tag.(get_id sample_1)];
            items = [Item.(get_id sample_1)];
            selected = Some (Item.(get_id sample_1));
      } in
      with_tag = expected

