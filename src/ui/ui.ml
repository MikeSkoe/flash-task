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

      let concat connect items = 
            let rec iter acc = function
                  | [] -> acc
                  | head :: tail -> iter (connect acc head) tail
            in
            iter I.empty items

      let ver = concat I.(<->)
      let hor = concat I.(<|>)

      let convert_style = function
            | Normal -> A.empty
            | Secondary -> A.(fg @@ gray 10)
            | Selected -> A.(bg white ++ fg black)
            | Underline -> A.(st underline)

      let text =
            convert_style
            >> I.string

      let multiline =
            String.split_on_char '\n'
            >> List.map @@ text Normal
            >> ver 
end

module UITag = struct
      let draw =
            Tag.get_title
            >> Printf.sprintf "#%s "
            >> UINode.(text Secondary)
end

module UIItem = struct
      let draw selected_item item =
            let tags =
                  Item.get_tags item
                  |> List.map UITag.draw
                  |> UINode.hor
            in
            let title =
                  Item.get_title item
                  |> UINode.(text (if selected_item = item then Selected else Normal))
            in
            I.(title <-> tags)
end

module UIFilter = struct
      let draw folder filter =
            let (selected_filter, selected_item) = Folder.get_selected folder in
            let is_selected = selected_filter = filter in
            let items =
                  Folder.get_items folder filter
                  |> List.map @@ UIItem.draw (if is_selected then selected_item else Item.empty)
                  |> UINode.ver 
            in
            let title =
                  Filter.get_name filter
                  |> UINode.(text (if is_selected then Underline else Normal))
            in
            I.(title <-> items)
end

module UIViewPage = struct
      let draw folder width = 
            let filters =
                  Folder.get_filters folder
                  |> List.map @@ (
                        UIFilter.draw folder
                        >> I.hsnap ~align:`Left (width / 3)
                        >> I.pad ~r:3
                  )
                  |> UINode.hor
            in
            filters
end

module UIDetailPage = struct
      let draw folder = 
            let (_, selected_item) = Folder.get_selected folder in
            let title =
                  Item.get_title selected_item
                  |> UINode.(text Normal)
            in
            let body =
                  Item.get_body selected_item
                  |> UINode.multiline
            in
            let divider = I.string A.empty "------" in
            I.(title <-> divider <-> body)
end

let draw_view folder =
      let (width, height) = Notty_unix.Term.size term in
      let view =
            UIViewPage.draw folder width
            |> I.vsnap ~align:`Top (height / 4)
      in
      let detail = UIDetailPage.draw folder in
      Notty_unix.Term.image term I.(view <-> detail);

      match Notty_unix.Term.event term with
            | `Key (`Escape, _) -> NavigationMsg Quit
            | `Key (`Enter, _) -> NavigationMsg ToDetail
            | `Key (`Arrow `Up, _) -> ViewMsg PrevItem
            | `Key (`Arrow `Down, _) -> ViewMsg NextItem
            | `Key (`Arrow `Left, _) -> ViewMsg PrevFilter
            | `Key (`Arrow `Right, _) -> ViewMsg NextFilter
            | _ -> NavigationMsg Nothing

let draw_detail folder =
      let view = UIDetailPage.draw folder in
      Notty_unix.Term.image term view;

      match Notty_unix.Term.event term with
            | `Key (`Escape, _) -> NavigationMsg ToView
            | `Key (`Arrow `Left, _) -> DetailMsg PrevItem
            | `Key (`Arrow `Right, _) -> DetailMsg NextItem
            | _ -> NavigationMsg Nothing


let draw = function
      | View folder -> draw_view folder
      | Detail folder -> draw_detail folder

