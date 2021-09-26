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
      | Init of Item.t Id.t * Item.t list
      | NextItem
      | PrevItem
      | Input of Textarea.msg
      | SaveItem

let rec iter_right (item: Item.t) = function
      | [] -> item
      | head :: next :: _ when Item.(get_id head) = item.id -> next 
      | _ :: tail -> iter_right item tail 

let rec iter_left (item: Item.t) = function
      | [] -> item
      | prev :: head :: _ when Item.(get_id head) = item.id -> prev 
      | _ :: tail -> iter_left item tail 

let shift_item right {id; items; _} =
      let cur_item = List.find (fun (item: Item.t) -> item.id = id) items in
      let iter = if right then iter_right else iter_left in
      let next = iter cur_item items in
      let textarea = Textarea.make Parser.(string_of_item next) in
      {id=next.id; items; textarea}

let save_item {id; items; textarea} =
      let items =
            items
            |> List.map (fun item ->
                  if Item.(get_id item) = id
                  then textarea.data
                        |> Parser.item_of_string 
                        |> Item.set_id id
                  else item
            )
      in
      {id; items; textarea}

let init_item item = Item.get_id item, Textarea.make Parser.(string_of_item item)

let update {id; items; textarea} = function
      | Init (id, items) ->
            let cur_item = List.find (fun (item: Item.t) -> item.id = id) items in
            let textarea = Textarea.make Parser.(string_of_item cur_item) in
            {id; items; textarea}
      | NextItem -> shift_item true {id; items; textarea}
      | PrevItem -> shift_item false {id; items; textarea}
      | SaveItem -> save_item {id; items; textarea}
      | Input msg ->
            let textarea = Textarea.update msg textarea in
            {id; items; textarea}

