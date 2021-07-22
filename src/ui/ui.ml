open Notty
open Notty_unix
open State

let term = Term.create ()

let draw_view (folder: Folder.t) =
      let fold_fn (curr: Item.t) (acc: I.t) =
          let style =
              if Some curr.id = folder.selected
              then A.(fg black ++ bg white)
              else A.empty
          in
          I.(acc <-> string style curr.title)
      in
      ItemSet.fold fold_fn folder.itemSet I.empty

let draw_review (folder: Folder.t) =
      let fold_fn (curr: Item.t) (acc: I.t) = I.(acc <-> string A.empty curr.title) in
      ItemSet.fold fold_fn folder.itemSet I.empty

let draw state =
      let view = match state with
            | View folder -> draw_view folder
            | Review folder -> draw_review folder
      in

      Notty_unix.Term.image term view;

      match Notty_unix.Term.event term with
            | `Key (`Escape, _) -> Quit
            | `Key (`Arrow `Up, _) -> Prev
            | `Key (`Arrow `Down, _) -> Next
            | _ -> Nothing

