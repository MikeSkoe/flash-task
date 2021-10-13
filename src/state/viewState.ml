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

module Make (Api: Api_type.T) = struct

      let shift_filter shift {items; filters; selected; input} =
            let selected =
                  selected
                  |> Selected.shift_filter filters shift
            in
            {items; filters; selected; input}

      let shift_item shift {items; filters; selected; input} =
            let selected =
                  selected
                  |> Selected.shift_item items shift
            in
            {items; filters; selected; input}

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
            let items = Api.ItemApi.get_all () in
            let filters = Filter.empty :: Api.FilterApi.get_all () in
            let selected = Selected.empty in
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
            | NextItem -> shift_item 1
            | PrevItem -> shift_item (-1)
            | Input msg -> change_input msg
end
