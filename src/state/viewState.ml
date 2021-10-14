open Entities
open For_ui
open Utils

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
 * type setter: 'a -> st -> st
 * type getter: st -> 'a
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
      let (>>=) = Select.(>>=)

      let get_filters {filters; _} = filters
      let get_selected {selected; _} = selected

      let items_len {items; _} = List.length items
      let filters_len {filters; _} = List.length filters

      let cur_filter =
          get_filters >>= fun filters ->
          get_selected >>= fun selected ->

          List.nth filters Selected.(Get.fi selected)
end

module Set = struct
      let selected selected st = { st with selected }
      let items items st = { st with items }
end

module Make (Api: Api_type.T) = struct
      let (>>=) = Select.(>>=)

      let delete_item (item: Item.t) {filters; selected; _} = 
            let _  = Api.ItemApi.delete item.id in
            let items = Api.ItemApi.get_all () in
            let selected =
                selected
                |> Selected.(shift_item List.(length items) 0 )
                |> Selected.(shift_filter List.(length filters) 0)
            in
            let input = Input.empty in
            {items; filters; selected; input}

      let delete_filter (filter: Filter.t) {items; selected; _} = 
            let _ = Api.FilterApi.delete filter.id in
            let filters = Filter.empty :: Api.FilterApi.get_all () in
            let selected =
                selected
                |> Selected.(shift_item List.(length items) 0 )
                |> Selected.(shift_filter List.(length filters) 0)
            in
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

      let shift_item shift =
            Get.items_len >>= fun items_len ->
            Get.selected >>= fun selected ->

            Selected.(shift_item items_len shift selected)
            |> Set.selected

      let shift_filter shift =
            Get.filters_len >>= fun filters_len ->
            Get.selected >>= fun selected ->

            Selected.(shift_filter filters_len shift selected)
                |> Set.selected

      let update = function
            | Init -> init
            | DeleteItem item -> delete_item item
            | DeleteFilter filter -> delete_filter filter
            | AddItem title -> add_item title
            | AddFilter title -> add_filter title
            | NextFilter -> shift_filter 1
            | PrevFilter -> shift_filter (-1)
            | NextItem -> shift_item 1
            | PrevItem -> shift_item (-1)
            | Input msg -> change_input msg
end

