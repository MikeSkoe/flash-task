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
      let draw item is_selected =
            let style =
                  if is_selected
                  then UINode.Selected
                  else UINode.Normal
            in
            let title =
                  Item.Get.title item
                  |> UINode.(text style)
            in
            title
end

module UIFilter = struct
      let draw filter is_selected =
            let title =
                  Filter.Get.title filter
                  |> UINode.(text (if is_selected then Underline else Normal))
            in
            title
end

module UIViewPage = struct
      let is_editing ({text; _}: Input.t) = not (text = "")

      let draw items filters (Selected.(Index fi, Index ii)) ({chr; text}: Input.t) = 
            let input = UINode.(editable chr Normal) text in
            let items =
                  items
                  |> List.mapi (fun i item -> UIItem.draw item (i = ii))
                  |> I.vcat 
            in
            let filters =
                  filters
                  |> List.mapi (fun i filter -> UIFilter.draw filter (i = fi)) 
                  |> List.map @@ (I.pad ~r:3)
                  |> I.vcat
            in
            I.(input <->
                  (filters <|> items))
end

module UIDetailPage = struct
      let image_of_title chr = function
            | 0 -> UINode.(editable chr Normal)
            | _ -> UINode.(text Normal)

      let image_of_body chr line =
            List.mapi (fun index str ->
                  if index + 1 = line
                  then UINode.(editable chr Normal str)
                  else UINode.(text Normal str)
            )
            >> I.vcat

      let draw ({pos; data}: Textarea.t) = 
            match String.split_on_char '\n' data with
            | [] ->
                  I.empty
            | title :: []
            | title :: "" :: [] ->
                  let (chr, line) = pos in
                  let title = image_of_title chr line title in 
                  title
            | title :: body ->
                  let (chr, line) = pos in
                  let title = image_of_title chr line title in 
                  let divider = UINode.(text Secondary "-----") in
                  let body = image_of_body chr line body in
                  I.(title <-> divider <-> body)
end

let draw_view items filters selected input =
      let view = UIViewPage.draw items filters selected input in
      Notty_unix.Term.image term view;
      let event = Notty_unix.Term.event term in

      if UIViewPage.is_editing input
      then begin match event with
            | `Key (`Escape, _) -> NavigationMsg Quit
            | `Key (`Enter, _) ->
                  begin match String.split_on_char ' ' input.text with
                  | ":delete"::_ ->
                        begin match selected with
                        | Selected.(Index _fi, Index ii) ->
                              try ViewMsg (DeleteItem List.(nth items ii))
                              with Failure _ -> NavigationMsg Nothing
                        end
                  | ":delete_filter"::_ ->
                        begin match selected with
                        | Selected.(Index fi, Index _ii) ->
                              try ViewMsg (DeleteFilter List.(nth filters fi))
                              with Failure _ -> NavigationMsg Nothing
                        end
                  | ":add"::text -> ViewMsg (AddItem String.(concat " " text))
                  | ":add_filter"::text -> ViewMsg (AddFilter String.(concat " " text))
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
                  | Selected.(Index _fi, Index ii) when ii >= 0 ->
                        let item = List.nth items ii in
                        NavigationMsg (ToDetail item.id)
                  | _ -> NavigationMsg Nothing
                  end
            | _ -> NavigationMsg Nothing
      end

let draw_detail textarea =
      let view = UIDetailPage.draw textarea in
      Notty_unix.Term.image term view;

      match Notty_unix.Term.event term with
            (* ui movement *)
            | `Key (`Arrow direction, modificators) ->
                  begin match direction, modificators with
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
      | Detail {textarea; _} -> draw_detail textarea
      | View {items; filters; selected; input} -> draw_view items filters selected input

