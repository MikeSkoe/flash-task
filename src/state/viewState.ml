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
      | ShiftSelected of int * int
      | DeleteItem
      | DeleteFilter
      | AddItem of string
      | AddFilter of string
      | Input of Input.msg

module Get = struct
      let (>>=) = Select.(>>=)
      let return = Select.return

      let items {items; _} = items
      let filters {filters; _} = filters
      let selected {selected; _} = selected
      let input {input; _} = input

      let items_len {items; _} = List.length items
      let filters_len {filters; _} = List.length filters

      let fi =
            filters_len >>= fun filters_len ->
            (selected >> Selected.Get.fi) >>= fun fi ->

            return (fi |> max 0 |> min (filters_len - 1))

      let ii =
            items_len >>= fun items_len ->
            (selected >> Selected.Get.ii) >>= fun ii ->

            return (ii |> max 0 |> min (items_len - 1))

      let cur_filter =
            filters >>= fun filters ->
            fi >>= fun fi ->

            return @@
                  try List.nth filters fi
                  with _ -> Filter.empty

      let cur_item =
            items >>= fun items ->
            ii >>= fun ii ->

            return @@
                  try List.nth items ii
                  with _ -> Item.empty
end

module Set = struct
      let selected selected st = { st with selected }
      let items items st = { st with items }
      let filters filters st = { st with filters }
      let input input st = { st with input }
end

module Make (Api: Api_type.T) = struct
      let (>>=) = Select.(>>=)

      let init _t = 
            let filters = Filter.empty :: Api.FilterApi.get_all () in
            let items = Api.ItemApi.get_via_filter Filter.empty in
            { empty with items; filters }

      let add_item title =
            let last_id = Api.ItemApi.last_id () in
            let item = Item.make ~id:(last_id + 1) ~title ~body:"" () in
            let _ = Api.ItemApi.add_or_replace item in
            id

      let add_filter title =
            let last_id = Api.FilterApi.last_id () in
            let filter = Filter.make ~id:(last_id + 1) ~title () in
            let _ = Api.FilterApi.add_or_replace filter in
            id

      let change_input msg {items; filters; selected; input} = 
            let input = Input.update msg input in
            {items; filters; selected; input}

      let delete_item =
            Get.cur_item >>= fun item ->
            let _  = Api.ItemApi.delete item.id in
            id

      let delete_filter = 
            Get.cur_filter >>= fun filter ->
            let _ = Api.FilterApi.delete filter.id in
            id

      let shift_selected (fi_shift, ii_shift) =
            Get.fi >>= fun fi ->
            Get.ii >>= fun ii ->

            Selected.make (fi, ii)
                |> Selected.shift fi_shift ii_shift
                |> Set.selected

      let update_filters st =
            let filters = Filter.empty :: Api.FilterApi.get_all () in
            Set.filters filters st

      let update_items st =
            let cur_filter = Get.cur_filter st in
            let items = Api.ItemApi.get_via_filter cur_filter in
            Set.items items st

      let update = function
            | Init -> init
            | DeleteItem -> delete_item
                  >> update_items
                  >> Set.input Input.empty
            | DeleteFilter -> delete_filter
                  >> update_filters
                  >> update_items
                  >> Set.input Input.empty
            | AddItem title -> add_item title
                  >> update_items
                  >> Set.input Input.empty
            | AddFilter title -> add_filter title
                  >> update_filters
                  >> Set.input Input.empty
            | ShiftSelected (fi, ii) -> shift_selected (fi, ii)
                  >> update_items
            | Input msg -> change_input msg
end
