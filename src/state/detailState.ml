open Entities
open For_ui

type t = {
      id: Item.t Id.t;
      textarea: Textarea.t;
}

let empty = {
      id=Id.empty;
      textarea=Textarea.empty;
}

type msg =
      | Init of Item.t Id.t
      | Input of Textarea.msg
      | SaveItem

module Make (Api: Api_type.T) = struct
      let rec iter_right (item: Item.t) = function
            | [] -> item
            | head :: next :: _ when Item.Get.(id head) = item.id -> next 
            | _ :: tail -> iter_right item tail 

      let rec iter_left (item: Item.t) = function
            | [] -> item
            | prev :: head :: _ when Item.Get.(id head) = item.id -> prev 
            | _ :: tail -> iter_left item tail 

      let save_item {id; textarea; _} =
            let item =
                  textarea.data
                  |> Parser.item_of_string id
            in
            let _ = Api.ItemApi.add_or_replace item in
            {id; textarea}

      let init id _t = 
            let items = Api.ItemApi.get_all () in
            let cur_item =
                  try List.find (fun (item: Item.t) -> item.id = id) items
                  with Not_found -> Item.empty
            in
            let textarea = Textarea.make Parser.(string_of_item cur_item) in
            {id; textarea}

      let change_input msg {id; textarea} = 
            let textarea = Textarea.update msg textarea in
            {id; textarea}

      let update = function
            | Init id -> init id
            | SaveItem -> save_item
            | Input msg -> change_input msg 
end
