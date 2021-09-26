open Entities
open Utils
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
      | NextItem
      | PrevItem
      | NextFilter
      | PrevFilter
      | DeleteItem of Item.t
      | DeleteFilter of Filter.t
      | Input of Input.msg
      | AddItem of string

let shift_filter shift {items; filters; selected; input} =
      let selected =
            selected
            |> Selected.shift_filter items filters shift
      in
      {items; filters; selected; input}

let shift_item shift {items; filters; selected; input} =
      let selected =
            selected
            |> Selected.shift_item items shift
      in
      {items; filters; selected; input}

let update {items; filters; selected; input} = function
      | DeleteItem item ->
            let items = List.filter (Item.(eq item) >> not) items in
            let selected = Selected.normalize filters items selected in
            let input = Input.empty in
            {items; filters; selected; input}
      | DeleteFilter filter ->
            let filters = List.filter (Filter.(eq filter) >> not) filters in
            let selected = Selected.normalize filters items selected in
            let input = Input.empty in
            {items; filters; selected; input}
      | AddItem title ->
            let tags =
                  Selected.get_filter selected
                  |> Filter.(Get.rule >> tags_of)
            in
            let item = Item.make title tags "" in
            let items = item :: items in
            let input = Input.empty in
            {items; filters; selected; input}
      | NextFilter -> shift_filter 1 {items; filters; selected; input}
      | PrevFilter -> shift_filter (-1) {items; filters; selected; input}
      | NextItem -> shift_item 1 {items; filters; selected; input}
      | PrevItem -> shift_item (-1) {items; filters; selected; input}
      | Input msg ->
            let input = Input.update msg input in
            {items; filters; selected; input}

