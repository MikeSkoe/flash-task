open Notty
open Notty_unix
open State

let term = Term.create ()

let draw_view (folder: Folder.t) =
      let filter = Folder.get_filter folder in
      let fold_fn (acc: I.t) (curr: Item.t) =
            let style =
                  if Filter.get_selected filter = Some Item.(get_id curr)
                  then A.(bg white ++ fg black)
                  else A.empty
            in
            I.(acc <-> string style Item.(get_title curr))
      in
      Folder.(get_items folder)
      |> List.fold_left fold_fn I.empty

let draw (View folder) =
      Notty_unix.Term.image term (draw_view folder);

      match Notty_unix.Term.event term with
            | `Key (`Escape, _) -> Quit
            | `Key (`Arrow `Up, _) -> Prev
            | `Key (`Arrow `Down, _) -> Next
            | _ -> Nothing

