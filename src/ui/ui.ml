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

let draw_item_detailed (folder: Folder.t) =
      let (_filter, item) = Folder.get_selected folder in
      let title = I.string A.empty Item.(get_title item) in
      let body_string =
            item
            |> Item.get_body
            |> String.to_seq
            |> List.of_seq
            |> List.map (function
                  | '\n' -> 'N'
                  | chr -> chr
            )
            |> List.to_seq 
            |> String.of_seq
      in
      let body = I.string A.empty body_string in
      let divider = I.string A.empty "------" in
      I.(title <-> divider <-> body)

let draw_view folder =
      let folder_title = I.string A.empty "VIEW SCHENE" in
      let view = draw_item_list folder in
      Notty_unix.Term.image term I.(folder_title <-> view);

      match Notty_unix.Term.event term with
            | `Key (`Escape, _) -> NavigationMsg Quit
            | `Key (`Enter, _) -> NavigationMsg ToDetail
            | `Key (`Arrow `Up, _) -> ViewMsg PrevItem
            | `Key (`Arrow `Down, _) -> ViewMsg NextItem
            | `Key (`Arrow `Left, _) -> ViewMsg PrevFilter
            | `Key (`Arrow `Right, _) -> ViewMsg NextFilter
            | _ -> NavigationMsg Nothing

let draw_detail folder =
      let folder_title = I.string A.empty "DETAIL SCHENE" in
      let view = draw_item_detailed folder in
      Notty_unix.Term.image term I.(folder_title <-> view);

      match Notty_unix.Term.event term with
            | `Key (`Escape, _) -> NavigationMsg ToView
            | `Key (`Arrow `Left, _) -> DetailMsg PrevItem
            | `Key (`Arrow `Right, _) -> DetailMsg NextItem
            | _ -> NavigationMsg Nothing


let draw = function
      | View folder -> draw_view folder
      | Detail folder -> draw_detail folder

