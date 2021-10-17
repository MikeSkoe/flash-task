open Notty
open For_ui
open Utils
open State

module UI = UiEntities

let image_of_title chr = function
      | 0 -> Node.(editable chr Normal)
      | _ -> Node.(text Normal)

let image_of_body chr line =
      List.mapi (fun index str ->
            if index + 1 = line
            then Node.(editable chr Normal str)
            else Node.(text Normal str)
      )
      >> I.vcat

let draw state = 
      let {Textarea.pos; Textarea.data} = DetailState.Get.textarea state in
      match String.split_on_char '\n' data with
      | [] ->
            I.empty
      | title :: [] ->
            let (chr, line) = pos in
            let title = image_of_title chr line title in 
            title
      | title :: body ->
            let (chr, line) = pos in
            let title = image_of_title chr line title in 
            let divider = Node.(text Secondary "-----") in
            let body = image_of_body chr line body in
            I.(title <-> divider <-> body)

let msg_of_event = function
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

