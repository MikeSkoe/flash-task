open Notty_unix
open Utils
open State

module UI = UiEntities

let term = Term.create ()

let (>>=) = Select.(>>=)
let return = Select.return

let draw_view =
      ViewPage.draw >>= fun view ->
      Notty_unix.Term.image term view;

      ViewPage.msg_of_event Term.(event term)

let draw_detail =
      DetailPage.draw >>= fun view ->
      Notty_unix.Term.image term view;
      
      return @@ DetailPage.msg_of_event Term.(event term)

let draw = function
      | Detail state -> draw_detail state
      | View state -> draw_view state

