type t =
      | NewItem of For_ui.Textarea.t
      | ExistingItem of Item.t Id.t * For_ui.Textarea.t

let map fn = function
      | NewItem textarea -> NewItem (fn textarea)
      | ExistingItem (id, textarea) -> ExistingItem (id, (fn textarea)) 
