open Notty
open Notty_unix
open State

let term = Term.create ()

let draw_view (folder: Folder.t) =
      let fold_fn (curr: Item.t) (acc: I.t) =
          I.(acc <-> string A.empty curr.title)
      in
      ItemSet.fold fold_fn folder.itemSet I.empty

let draw (View (folder, _)) =
      Notty_unix.Term.image term (draw_view folder);

      match Notty_unix.Term.event term with
            | `Key (`Escape, _) -> Quit
            | `Key (`Arrow `Up, _) -> Prev
            | `Key (`Arrow `Down, _) -> Next
            | _ -> Nothing

