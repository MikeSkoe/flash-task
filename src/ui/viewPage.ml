open Notty
open For_ui
open Utils
open State

module UI = UiEntities

let (>>=) = Select.(>>=)
let return = Select.return

let is_editing ({text; _}: Input.t) = not (text = "")

let draw_items = 
      ViewState.Get.items >>= fun items ->
      ViewState.Get.ii >>= fun ii ->

      return (
            items
            |> List.mapi (fun i item -> UI.Item.draw ~item ~is_selected:(i = ii))
            |> I.vcat
      )

let draw_filters =
      ViewState.Get.filters >>= fun filters ->
      ViewState.Get.fi >>= fun fi ->

      return (
            filters
            |> List.mapi (fun i filter -> UI.Filter.draw ~filter ~is_selected:(i = fi)) 
            |> List.map @@ (I.pad ~r:3)
            |> I.vcat
      )

let draw_input =
      ViewState.Get.input >>= fun input ->

      return Node.(editable input.chr Normal input.text)

let draw state = 
      let input = draw_input state in
      let items = draw_items state in
      let filters = draw_filters state in

      I.(input <-> (filters <|> items))

let msg_of_event event state =
      let input = ViewState.Get.input state in

      if is_editing input
      then begin match event with
            | `Key (`Escape, _) -> NavigationMsg Quit
            | `Key (`Enter, _) ->
                  begin match String.split_on_char ' ' input.text with
                  | ":delete"::_ -> ViewMsg DeleteItem
                  | ":delete_filter"::_ -> ViewMsg DeleteFilter
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
                  | `Up -> ViewMsg (ShiftSelected (0, -1))
                  | `Down -> ViewMsg (ShiftSelected (0, 1))
                  | `Left -> ViewMsg (ShiftSelected (-1, 0))
                  | `Right -> ViewMsg (ShiftSelected (1, 0))
            end

            (* navigation *)
            | `Key (`Escape, _) -> NavigationMsg Quit
            | `Key (`ASCII ':', _) -> ViewMsg (Input Input.(TypeChar ':'))
            | `Key (`Enter, _) -> NavigationMsg (ToDetail ViewState.(Get.cur_item state).id)
            | _ -> NavigationMsg Nothing
      end

