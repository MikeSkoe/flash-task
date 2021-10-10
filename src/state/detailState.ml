open Entities
open For_ui

type t = {
      id: Item.t Id.t;
      items: Item.t list;
      textarea: Textarea.t;
}

let empty = {
      id=Id.empty;
      items=[];
      textarea=Textarea.empty;
}

type msg =
      | Init of Item.t Id.t
      | NextItem
      | PrevItem
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

      let shift_item right {id; items; _} =
            let cur_item = List.find (fun (item: Item.t) -> item.id = id) items in
            let iter = if right then iter_right else iter_left in
            let next = iter cur_item items in
            let textarea = Textarea.make Parser.(string_of_item next) in
            {id=next.id; items; textarea}

      let save_item {id; textarea; _} =
            let item =
                  textarea.data
                  |> Parser.item_of_string 
                  |> Item.Set.id id
            in
            let _ = Api.ItemApi.add_or_replace item in
            let items = Api.ItemApi.get_all () in
            {id; items; textarea}

      let init id _t = 
            let items = Api.ItemApi.get_all () in
            let cur_item =
                  try List.find (fun (item: Item.t) -> item.id = id) items
                  with Not_found -> Item.empty
            in
            let textarea = Textarea.make Parser.(string_of_item cur_item) in
            {id; items=[]; textarea}

      let change_input msg {id; items; textarea} = 
            let textarea = Textarea.update msg textarea in
            {id; items; textarea}

      let update = function
            | Init id -> init id
            | NextItem -> shift_item true
            | PrevItem -> shift_item false
            | SaveItem -> save_item
            | Input msg -> change_input msg 
end
