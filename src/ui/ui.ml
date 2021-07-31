open Notty
open Notty_unix
open State

let term = Term.create ()

let draw_tags ~tags =
      let fold acc curr =
            let tag =
                  let title = Tag.(get_title curr) in
                  I.(string A.empty Printf.(sprintf " #%s" title))
            in
            I.(acc <|> tag)
      in
      List.fold_left fold I.empty tags

let draw_item (filter: Filter.t) (item: Item.t) (selected: Filter.t * Item.t) =
      let style =
            match selected with
            | (selected_filter, selected_item) when selected_filter = filter && selected_item = item ->
                  A.(bg white ++ fg black)
            | _ ->
                  A.empty
      in
      let title = I.(string style Item.(get_title item)) in
      let tags = draw_tags ~tags:Item.(get_tags item) in
      I.(title <|> tags)

let draw_item_list (folder: Folder.t) =
      let selected = Folder.get_selected folder in
      let fold (filter: Filter.t) (acc: I.t) (item: Item.t) =
            I.(acc <-> draw_item filter item selected)
      in
      let filtered =
            Folder.get_filters folder
            |> List.map (fun filter ->
                  let items =
                        Folder.get_items folder filter
                        |> List.fold_left (fold filter) I.empty
                  in
                  let title = Filter.get_name filter |> I.string A.empty in
                  I.(title <-> items)
            )
            |> List.fold_left I.(<|>) I.empty
      in
      filtered

let draw (View folder) =
      Notty_unix.Term.image term (draw_item_list folder);

      match Notty_unix.Term.event term with
            | `Key (`Escape, _) -> Quit
            | `Key (`Arrow `Up, _) -> PrevItem
            | `Key (`Arrow `Down, _) -> NextItem
            | `Key (`Arrow `Left, _) -> PrevFilter
            | `Key (`Arrow `Right, _) -> NextFilter
            | _ -> Nothing

