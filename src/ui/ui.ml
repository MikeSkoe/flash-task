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

      let convert_style = function
            | Normal -> A.empty
            | Secondary -> A.(fg @@ gray 10)
            | Selected -> A.(bg white ++ fg black)
            | Underline -> A.(st underline)

      let text = convert_style >> I.string

      let editable cursor style str =
            let style = convert_style style in
            let (before, curr, after) = For_ui.Textarea.split_on_pos cursor str in
            let before = I.string style before in
            let curr = I.string (convert_style Selected) curr in
            let after = I.string style after in
            I.(before <|> curr <|> after)
end

module UIInput = struct
      let draw (_chr, _line) =
            String.split_on_char '\n'
            >> List.map @@ UINode.text Normal
            >> I.vcat
end

module UITag = struct
      let draw =
            Tag.get_title
            >> Printf.sprintf "[%s]"
            >> UINode.(text Secondary)
end

module UIItem = struct
      let draw is_folder_selected selected item =
            let tags =
                  Item.get_tags item
                  |> List.map UITag.draw
                  |> I.hcat
            in
            let style = match (is_folder_selected, selected) with
                  | (true, Selected.Item (_, selected_item)) when selected_item = item -> UINode.Selected
                  | _ -> UINode.Normal
            in
            let title =
                  Item.get_title item
                  |> UINode.(text style)
            in
            I.(title <-> tags)
end

module UIFilter = struct
      let draw folder selected filter =
            let is_selected = match selected with
                  | Selected.Filter selected_filter when selected_filter = filter -> true
                  | Selected.Item (selected_filter, _) when selected_filter = filter -> true
                  | _ -> false
            in
            let items =
                  Folder.get_items folder filter
                  |> List.map @@ UIItem.draw is_selected selected
                  |> I.vcat 
            in
            let title =
                  Filter.get_title filter
                  |> UINode.(text (if is_selected then Underline else Normal))
            in
            let rule =
                  Filter.get_rule filter
                  |> Parser.string_of_rule
                  |> UINode.(text Secondary)
                  |> I.pad ~b:1
            in
            I.(title <-> rule <-> items)
end

module UIViewPage = struct
      let draw folder selected width = 
            let filters =
                  Folder.get_filters folder
                  |> List.map @@ (
                        UIFilter.draw folder selected
                        >> I.hsnap ~align:`Left (width / 3)
                        >> I.pad ~r:3
                  )
                  |> I.hcat
            in
            filters
end

module UIDetailPage = struct
      let draw _folder ({pos; data} : For_ui.Textarea.t) = 
            let image_of_title chr = function
                  | 0 -> UINode.(editable chr Normal)
                  | _ -> UINode.(text Normal)
            in
            let image_of_tags chr = function
                  | 1 -> UINode.(editable chr Secondary)
                  | _ -> UINode.(text Secondary)
            in
            let image_of_body chr line =
                  List.mapi (fun index str ->
                        if index + 2 = line
                        then UINode.(editable chr Normal str)
                        else UINode.(text Normal str)
                  )
                  >> I.vcat
            in
            match String.split_on_char '\n' data with
            | [] ->
                  I.empty
            | title :: []
            | title :: "" :: [] ->
                  let (chr, line) = pos in
                  let title = image_of_title chr line title in 
                  title
            | title :: tags :: []
            | title :: tags :: "" :: [] ->
                  let (chr, line) = pos in
                  let title = image_of_title chr line title in 
                  let tags = image_of_tags chr line tags in
                  I.(title <-> tags)
            | title :: tags :: body ->
                  let (chr, line) = pos in
                  let title = image_of_title chr line title in 
                  let tags = image_of_tags chr line tags in
                  let divider = UINode.(text Secondary "-----") in
                  let body = image_of_body chr line body in
                  I.(title <-> tags <-> divider <-> body)
end

(* --- *)

let draw_view folder selected =
      let (width, _) = Notty_unix.Term.size term in
      let view = UIViewPage.draw folder selected width in
      Notty_unix.Term.image term view;

      match Notty_unix.Term.event term with
            | `Key (`Arrow `Up, _) -> ViewMsg PrevItem
            | `Key (`Arrow `Down, _) -> ViewMsg NextItem
            | `Key (`Arrow `Left, _) -> ViewMsg PrevFilter
            | `Key (`Arrow `Right, _) -> ViewMsg NextFilter
            | `Key (`Backspace, _) -> 
                  begin match selected with
                  | Selected.Item (_, selected_item) ->
                        ViewMsg (DeleteItem selected_item)
                  | _ -> NavigationMsg Nothing
                  end
            | `Key (`ASCII ' ', _) ->
                  let filter = match selected with
                        | Selected.Item (selected_filter, _) -> selected_filter
                        | Selected.Filter selected_filter -> selected_filter
                  in
                  let tags = match Filter.get_rule filter with
                        | Filter.WithoutTags -> []
                        | Filter.All -> []
                        | Filter.OptTag rule_items ->
                              List.fold_left (fun tags rule_item ->
                                    begin match rule_item with
                                    | Filter.WithTag tag -> tags @ [tag]
                                    end
                              ) [] rule_items
                  in
                  let item = Item.(set_tags tags empty) in
                  NavigationMsg (ToDetail item)
            | `Key (`Enter, _) -> 
                  begin match selected with
                  | Selected.Item (_, selected_item) -> 
                        NavigationMsg (ToDetail selected_item)
                  | _ -> NavigationMsg Nothing
                  end
            | `Key (`Escape, _) -> NavigationMsg Quit
            | _ -> NavigationMsg Nothing

let draw_detail folder edit_data id =
      let view = UIDetailPage.draw folder edit_data in
      Notty_unix.Term.image term view;

      match Notty_unix.Term.event term with
            | `Key (`Tab, _) -> 
                    let item =
                          Parser.item_of_string edit_data.data
                          |> Item.set_id id
                    in
                    NavigationMsg (Save (folder, item))

            | `Key (`Arrow `Left, _) -> DetailMsg (ShiftCursor (-1, 0))
            | `Key (`Arrow `Right, _) -> DetailMsg (ShiftCursor (1, 0))
            | `Key (`Arrow `Up, _) -> DetailMsg (ShiftCursor (0, -1))
            | `Key (`Arrow `Down, _) -> DetailMsg (ShiftCursor (0, 1))
            | `Key (`ASCII chr, _) -> DetailMsg (TypeChar chr)
            | `Key (`Backspace, _) -> DetailMsg DelChar
            | `Key (`Enter, _) -> DetailMsg (TypeChar '\n')

            | `Key (`Escape, _) -> NavigationMsg ToView
            | _ -> NavigationMsg Nothing

let draw = function
      | View (folder, selected) -> draw_view folder selected
      | Detail (folder, edit_data, id) -> draw_detail folder edit_data id

