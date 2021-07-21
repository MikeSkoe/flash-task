open Notty
open Notty_unix

let term = Term.create ()

let draw_view (folder: State.Folder.t) =
      let fold_fn (curr: State.Item.t) (acc: I.t) = I.(acc <-> string A.empty curr.title) in
      State.ItemSet.fold fold_fn folder.itemSet I.empty

let draw_review (folder: State.Folder.t) =
      let fold_fn (curr: State.Item.t) (acc: I.t) = I.(acc <-> string A.empty curr.title) in
      State.ItemSet.fold fold_fn folder.itemSet I.empty

let draw state =
      let view = match state with
            | State.View folder -> draw_view folder
            | State.Review folder -> draw_review folder
      in

      Notty_unix.Term.image term view;

      match Notty_unix.Term.event term with
            | `Key (`Escape, _) -> State.Quit
            | _ -> State.Nothing

