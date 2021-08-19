open For_ui

type t =
      | NewItem of Textarea.t
      | ExistingItem of Item.t Id.t * Textarea.t

let map fn = function
      | NewItem textarea -> NewItem (fn textarea)
      | ExistingItem (id, textarea) -> ExistingItem (id, (fn textarea)) 

let of_item item =
      let id = Item.(get_id item) in
      let textarea = Textarea.make @@ Parser.string_of_item item in
      ExistingItem (id, textarea)

let of_filter filter =
      let tags = Filter.(tags_of @@ get_rule filter) in
      let item = Item.(make "" tags "") in
      NewItem Textarea.(make @@ Parser.string_of_item item)

let item_of = function
      | ExistingItem (id, textarea) -> 
              Parser.item_of_string textarea.data
              |> Item.set_id id
      | NewItem textarea -> 
              Parser.item_of_string textarea.data

