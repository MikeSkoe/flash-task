open For_ui

type t =
      | NewItem of Textarea.t
      | ExistingItem of Item.t Id.t * Textarea.t

      | NewFilter of Textarea.t
      | ExistingFilter of Filter.t Id.t * Textarea.t

let empty = NewItem Textarea.empty

let map fn = function
      | NewItem textarea -> NewItem (fn textarea)
      | ExistingItem (id, textarea) -> ExistingItem (id, fn textarea)

      | NewFilter textarea -> NewFilter (fn textarea)
      | ExistingFilter (id, textarea) -> ExistingFilter (id, fn textarea)

let of_item item =
      let id = Item.(get_id item) in
      let textarea = Textarea.make @@ Parser.string_of_item item in
      ExistingItem (id, textarea)

let of_rule rule =
      let tags = Filter.(tags_of rule) in
      let item = Item.(make "" tags "") in
      let textarea = Textarea.make @@ Parser.string_of_item item in
      NewItem textarea

let of_filter filter =
      let textarea = Textarea.make @@ Parser.string_of_filter filter in
      let id = Filter.get_id filter in
      ExistingFilter (id, textarea)

let item_of = function
      | NewItem textarea -> 
              Parser.item_of_string textarea.data
      | ExistingItem (id, textarea) -> 
              Parser.item_of_string textarea.data
              |> Item.set_id id
      | NewFilter _ -> Item.empty
      | ExistingFilter _ -> Item.empty

let filter_of = function
      | NewItem _ -> Filter.empty
      | ExistingItem _ -> Filter.empty
      | NewFilter textarea ->
            Parser.filter_of_string textarea.data
      | ExistingFilter (id, textarea) ->
            Parser.filter_of_string textarea.data
            |> Filter.set_id id

