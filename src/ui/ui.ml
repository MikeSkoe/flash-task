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

      let text ?cursor:(cursor=(-1)) style str =
            let style = convert_style style in
            match cursor with
            | -1 ->
                  I.string style str
            | pos ->
                  let (before, curr, after) = For_ui.Input.split_on_pos pos str in
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
      let draw selected_item item =
            let tags =
                  Item.get_tags item
                  |> List.map UITag.draw
                  |> I.hcat
            in
            let title =
                  Item.get_title item
                  |> UINode.(text (if selected_item = item then Selected else Normal))
            in
            I.(title <-> tags)
            |> I.pad ~l:1 ~r:0 ~t:0 ~b:1
end

module UIFilter = struct
      let draw folder filter =
            let (selected_filter, selected_item) = Folder.get_selected folder in
            let is_selected = selected_filter = filter in
            let items =
                  Folder.get_items folder filter
                  |> List.map @@ UIItem.draw (if is_selected then selected_item else Item.empty)
                  |> I.vcat 
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
                  |> I.hcat
            in
            filters
end

module UIDetailPage = struct
      let draw _folder ({pos; data} : For_ui.Input.t) = 
            match String.split_on_char '\n' data with
            | [] ->
                  I.empty
            | title :: [] ->
                  let (chr, line) = pos in
                  let title =
                        if line = 0
                        then UINode.(text ~cursor:chr Normal title)
                        else UINode.(text Normal title)
                  in 
                  title
            | title :: tags :: [] ->
                  let (chr, line) = pos in
                  let title =
                        if line = 0
                        then UINode.(text ~cursor:chr Normal title)
                        else UINode.(text Normal title)
                  in 
                  let tags = UINode.(text Secondary tags) in
                  I.(title <-> tags)
            | title :: tags :: body ->
                  let (chr, line) = pos in
                  let title =
                        if line = 0
                        then UINode.(text ~cursor:chr Normal title)
                        else UINode.(text Normal title)
                  in 
                  let tags = 
                      if line = 1
                      then UINode.(text ~cursor:chr Secondary tags)
                      else UINode.(text Secondary tags)
                  in
                  let divider = UINode.(text Secondary "-----") in
                  let body =
                        body
                        |> List.mapi (fun index str ->
                              if index + 2 = line
                              then UINode.(text ~cursor:chr Normal str)
                              else UINode.(text Normal str)
                        )
                        |> I.vcat
                  in
                  I.(title <-> tags <-> divider <-> body)
end

(* --- *)

let draw_view folder =
      let (width, height) = Notty_unix.Term.size term in
      let view =
            UIViewPage.draw folder width
            |> I.vsnap ~align:`Top (height / 4)
      in
      Notty_unix.Term.image term view;

      match Notty_unix.Term.event term with
            | `Key (`Arrow `Up, _) -> ViewMsg PrevItem
            | `Key (`Arrow `Down, _) -> ViewMsg NextItem
            | `Key (`Arrow `Left, _) -> ViewMsg PrevFilter
            | `Key (`Arrow `Right, _) -> ViewMsg NextFilter

            | `Key (`Escape, _) -> NavigationMsg Quit
            | `Key (`Enter, _) -> NavigationMsg ToDetail
            | _ -> NavigationMsg Nothing

let draw_detail folder edit_data =
      let view = UIDetailPage.draw folder edit_data in
      Notty_unix.Term.image term view;

      match Notty_unix.Term.event term with
            | `Key (`Tab, _) -> 
                    let item = Parser.item_of_string edit_data.data in
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
      | View folder -> draw_view folder
      | Detail (folder, edit_data) -> draw_detail folder edit_data

