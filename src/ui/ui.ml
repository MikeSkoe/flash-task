open Notty
open Notty_unix
open State

let term = Term.create ()

let draw_item (item: Item.t) selected =
      let style =
            match selected with
            | Some id when id = Item.(get_id item) ->
                  A.(bg white ++ fg black)
            | _ ->
                  A.empty
      in
      I.(string style Item.(get_title item))

let draw_item_list (folder: Folder.t) =
      let filter = Folder.get_filter folder in
      let selected = Filter.get_selected filter in
      let fold (acc: I.t) (item: Item.t) =
            I.(acc <-> draw_item item selected)
      in
      Folder.(get_items folder)
      |> List.fold_left fold I.empty

let draw (View folder) =
      Notty_unix.Term.image term (draw_item_list folder);

      match Notty_unix.Term.event term with
            | `Key (`Escape, _) -> Quit
            | `Key (`Arrow `Up, _) -> Prev
            | `Key (`Arrow `Down, _) -> Next
            | _ -> Nothing

