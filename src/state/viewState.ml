open Entities
open For_ui

type t = {
      items: Item.t list;
      filters: Filter.t list;
      selected: Selected.t;
      input: Input.t;
}

let empty = {
      items=[];
      filters=[];
      selected=Selected.empty;
      input=Input.empty;
}

type msg = 
      | Init
      | NextItem
      | PrevItem
      | NextFilter
      | PrevFilter
      | DeleteItem of Item.t
      | DeleteFilter of Filter.t
      | Input of Input.msg
      | AddItem of string
      | AddFilter of string

(*
 * type setter: arg -> st * 'a -> st
 * type getter: st * 'a -> st
 *
 * let set_st_ii =
 *     get_arg >>= fun shift ->
 *     get_ii >>= fun ii ->
 *     get_items >>= fun items ->
 *     let ii =
 *         (ii + shift)
 *         |> max 0
 *         |> min List.(length items - 1)
 *     in
 *     set_ii ii;;
 *)

module Get = struct
      let get_items ({items; _}, _) = items
      let get_selected ({selected; _}, _) = selected
      let get_ii ({selected; _}, _) = Selected.Get.(ii (selected, ()))
end

module Set = struct
      module S = Utils.Select
      let (>>=) = S.(>>=)

      let set_selected selected (st, _) = { st with selected }

      let shift_item =
            S.get_arg >>= fun shift ->
            Get.get_items >>= fun items ->
            Get.get_ii >>= fun ii ->
            Get.get_selected >>= fun selected ->
            let ii = ii + shift |> max 0 |> min List.(length items - 1) in 
            let selected = Selected.Set.ii ii (selected, ()) in
            set_selected selected
end

module Make (Api: Api_type.T) = struct
      let shift_filter shift {filters; selected; input; _} =
            let selected =
                  selected
                  |> Selected.shift_filter filters shift
            in
            let cur_filter =
                  match selected with
                  | Selected.(Index fi, Index _ii) -> List.nth filters fi
            in
            let items = Api.ItemApi.get_via_filter cur_filter  in
            {items; filters; selected; input}

        (*
      let shift_item shift {items; filters; selected; input} =
            let selected =
                  selected
                  |> Selected.shift_item items shift
            in
            {items; filters; selected; input}
*)

      let delete_item (item: Item.t) {filters; selected; _} = 
            let _  = Api.ItemApi.delete item.id in
            let items = Api.ItemApi.get_all () in
            let selected = Selected.normalize filters items selected in
            let input = Input.empty in
            {items; filters; selected; input}

      let delete_filter (filter: Filter.t) {items; selected; _} = 
            let _ = Api.FilterApi.delete filter.id in
            let filters = Filter.empty :: Api.FilterApi.get_all () in
            let selected = Selected.normalize filters items selected in
            let input = Input.empty in
            {items; filters; selected; input}

      let add_item title {filters; selected; _} =
            let last_id = Api.ItemApi.last_id () in
            let item = Item.make ~id:(last_id + 1) ~title ~body:"" () in
            let _ = Api.ItemApi.add_or_replace item in
            let items = Api.ItemApi.get_all () in
            let input = Input.empty in
            {items; filters; selected; input}

      let add_filter title {items; selected; _} =
            let last_id = Api.ItemApi.last_id () in
            let filter = Filter.make ~id:(last_id + 1) ~title () in
            let _ = Api.FilterApi.add_or_replace filter in
            let filters = Filter.empty :: Api.FilterApi.get_all () in
            let input = Input.empty in
            {items; filters; selected; input}

      let change_input msg {items; filters; selected; input} = 
            let input = Input.update msg input in
            {items; filters; selected; input}

      let init _t = 
            let selected = Selected.empty in
            let filters = Filter.empty :: Api.FilterApi.get_all () in
            let items = Api.ItemApi.get_via_filter Filter.empty in
            let input = Input.empty in
            { items; filters; selected; input }

      let update = function
            | Init -> init
            | DeleteItem item -> delete_item item
            | DeleteFilter filter -> delete_filter filter
            | AddItem title -> add_item title
            | AddFilter title -> add_filter title
            | NextFilter -> shift_filter 1
            | PrevFilter -> shift_filter (-1)
            | NextItem -> (fun st -> Set.shift_item (st, 1))
            | PrevItem -> (fun st -> Set.shift_item (st, -1))
            | Input msg -> change_input msg
end

