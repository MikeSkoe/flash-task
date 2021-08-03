open Notty
open Notty_unix
open State
open Utils

let term = Term.create ()

module UINode = struct
      type style =
            | Normal
            | Secondary
            | Selected
            | Underline

      let text style =
            let style = match style with
                  | Normal -> A.empty
                  | Secondary -> A.(fg @@ gray 10)
                  | Selected -> A.(bg white ++ fg black)
                  | Underline -> A.(st underline)
            in
            I.string style

      let concat connect items = 
            let rec iter acc = function
                  | [] -> acc
                  | head :: tail -> iter (connect acc head) tail
            in
            iter I.empty items

      let ver = concat I.(<->)
      let hor = concat I.(<|>)
end

module UITag = struct
      let draw =
            Tag.get_title
            >> Printf.sprintf "#%s "
            >> UINode.(text Secondary)
end

module UIItem = struct
      let draw selected item =
            let tags =
                  Item.get_tags item
                  |> List.map UITag.draw
                  |> UINode.hor
            in
            let style =
                  if selected = item
                  then UINode.Selected
                  else UINode.Normal
            in
            let title =
                  Item.get_title item
                  |> UINode.(text style)
            in
            I.(title <-> tags)
end

let draw_filter_title (filter: Filter.t) (selected_filter: Filter.t) = 
      let title = Filter.get_name filter in
      let style =
            if filter = selected_filter
            then A.(st underline)
            else A.empty
      in
      I.string style title

let draw_item_list (folder: Folder.t): I.t =
      let (selected_filter, selected_item) = Folder.get_selected folder in
      let filtered =
            Folder.get_filters folder
            |> List.map (fun filter ->
                  let items =
                        Folder.get_items folder filter
                        |> List.map @@ UIItem.draw selected_item
                        |> UINode.ver 
                  in
                  let title = draw_filter_title filter selected_filter in
                  I.(title <-> items)
            )
            |> UINode.hor
      in
      filtered

let draw_item_detailed (folder: Folder.t) =
      let (_filter, item) = Folder.get_selected folder in
      let title = I.string A.empty Item.(get_title item) in
      let body =
            item
            |> Item.get_body
            |> String.split_on_char '\n'
            |> List.map @@ I.string A.empty
            |> List.fold_left I.(<->) I.empty
      in
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

