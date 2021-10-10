open Notty
open Notty_unix
open Utils
open Entities
open State
open For_ui

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
            let (before, curr, after) = Textarea.split_on_pos cursor str in
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
            Tag.as_string
            >> Printf.sprintf "[%s]"
            >> UINode.(text Secondary)
end

module UIItem = struct
      let draw is_folder_selected selected item =
            let tags =
                  Item.Get.tags item
                  |> List.map UITag.draw
                  |> I.hcat
            in
            let style = match (is_folder_selected, selected) with
                  | (true, Selected.Item (_, selected_item)) when Item.eq selected_item item -> UINode.Selected
                  | _ -> UINode.Normal
            in
            let title =
                  Item.Get.title item
                  |> UINode.(text style)
            in
            I.(title <-> tags)
end

module UIFilter = struct
      let draw items selected filter =
            let is_selected = match selected with
                  | Selected.Filter selected_filter when Filter.eq selected_filter filter -> true
                  | Selected.Item (selected_filter, _) when Filter.eq selected_filter filter -> true
                  | _ -> false
            in
            let items =
                  items
                  |> Filter.apply filter
                  |> List.map @@ UIItem.draw is_selected selected
                  |> I.vcat 
            in
            let title =
                  Filter.Get.title filter
                  |> UINode.(text (if is_selected then Underline else Normal))
            in
            let rule =
                  Filter.Get.rule filter
                  |> Parser.string_of_rule
                  |> UINode.(text Secondary)
                  |> I.pad ~b:1
            in
            I.(title <-> rule <-> items)
end

module UIViewPage = struct
      let is_editing ({text; _}: Input.t) = not (text = "")

      let draw items filters selected ({chr; text}: Input.t) width = 
            let input = UINode.(editable chr Normal) text in
            let filters =
                  filters
                  |> List.map @@ (
                        UIFilter.draw items selected
                        >> I.hsnap ~align:`Left (width / 3)
                        >> I.pad ~r:3
                  )
                  |> I.hcat
            in
            I.(input <-> filters)
end

module UIDetailPage = struct
      let draw ({pos; data}: Textarea.t) = 
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

let draw_view items filters selected input =
      let (width, _) = Notty_unix.Term.size term in
      let view = UIViewPage.draw items filters selected input width in
      Notty_unix.Term.image term view;
      let event = Notty_unix.Term.event term in

      if UIViewPage.is_editing input
      then begin match event with
            | `Key (`Escape, _) -> NavigationMsg Quit
            | `Key (`Enter, _) ->
                  begin match String.split_on_char ' ' input.text with
                  | ":delete"::_ ->
                        begin match selected with
                        | Selected.Item (_filter, item) -> ViewMsg (DeleteItem item)
                        | Selected.Filter filter -> ViewMsg (DeleteFilter filter)
                        end
                  | ":add"::text -> ViewMsg (AddItem String.(concat " " text))
                  | ":quit"::_ -> NavigationMsg Quit
                  | _ -> NavigationMsg Nothing
                  end
            | `Key (`ASCII chr, _) -> ViewMsg (Input Input.(TypeChar chr))
            | `Key (`Backspace, _) -> ViewMsg (Input Input.DelChar)
            | _ -> NavigationMsg Nothing
      end
      else begin match event with
            (* ui movement *)
            | `Key (`Arrow direction, _) -> begin match direction with
                  | `Up -> ViewMsg PrevItem
                  | `Down -> ViewMsg NextItem
                  | `Left -> ViewMsg PrevFilter
                  | `Right -> ViewMsg NextFilter
            end

            (* navigation *)
            | `Key (`Escape, _) -> NavigationMsg Quit
            | `Key (`ASCII ':', _) -> ViewMsg (Input Input.(TypeChar ':'))
            | `Key (`Enter, _) -> 
                  begin match selected with
                  | Selected.Item (_selected_filter, selected_item) ->
                        NavigationMsg (ToDetail (Item.(Get.id selected_item)))
                  | _ -> NavigationMsg Nothing
                  end
            | _ -> NavigationMsg Nothing
      end

let draw_detail _items textarea =
      let view = UIDetailPage.draw textarea in
      Notty_unix.Term.image term view;

      match Notty_unix.Term.event term with
            (* ui movement *)
            | `Key (`Arrow direction, modificators) ->
                  begin match direction, modificators with
                  | `Right, [`Shift] -> DetailMsg NextItem
                  | `Left, [`Shift] -> DetailMsg PrevItem
                  | `Left, _ -> DetailMsg (Input Textarea.(ShiftCursor (-1, 0)))
                  | `Right, _ -> DetailMsg (Input Textarea.(ShiftCursor (1, 0)))
                  | `Up, _ -> DetailMsg (Input Textarea.(ShiftCursor (0, -1)))
                  | `Down, _ -> DetailMsg (Input Textarea.(ShiftCursor (0, 1)))
                  end
            (* typing *)
            | `Key (`ASCII chr, _) -> DetailMsg (Input Textarea.(TypeChar chr))
            | `Key (`Backspace, _) -> DetailMsg (Input Textarea.DelChar)
            | `Key (`Enter, _) -> DetailMsg (Input Textarea.(TypeChar '\n'))
            | `Key (`Tab, _) -> DetailMsg SaveItem
            (* navigation *)
            | `Key (`Escape, _) -> NavigationMsg ToView
            | _ -> NavigationMsg Nothing

let draw = function
      | Detail {items; textarea; _} -> draw_detail items textarea
      | View {items; filters; selected; input} -> draw_view items filters selected input

