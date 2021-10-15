open Notty_unix
open Utils
open State

module UI = UiEntities

let term = Term.create ()

let (>>=) = Select.(>>=)
let return = Select.return

let draw_view (state: ViewState.t) =
      let view = ViewPage.draw state in
      Notty_unix.Term.image term view;

      Notty_unix.Term.event term
      |> ViewPage.msg_of_event state

let draw_detail (state: DetailState.t) =
      let view = DetailPage.draw state.textarea in
      Notty_unix.Term.image term view;

      Notty_unix.Term.event term
      |> DetailPage.msg_of_event


let draw = function
      | Detail state -> draw_detail state
      | View state -> draw_view state

