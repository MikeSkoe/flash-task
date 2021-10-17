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

module Get = struct
      let id {id; _} = id

      let item {id; textarea} = textarea.data |> Parser.item_of_string id

      let upd_textarea msg {textarea; _} = Textarea.update msg textarea

      let textarea {textarea; _} = textarea
end

module Set = struct
      let textarea textarea st = {st with textarea}
end

module Make (Api: Api_type.T) = struct
      let (>>=) = Select.MapFn.(>>=)
      let id = Select.MapFn.id

      let save_item =
            Get.item >>= fun item ->
            let _ = Api.ItemApi.add_or_replace item in
            id

      let init id _t = 
            let items = Api.ItemApi.get_all () in
            let cur_item =
                  try List.find (fun (item: Item.t) -> item.id = id) items
                  with Not_found -> Item.empty
            in
            let textarea = Textarea.make Parser.(string_of_item cur_item) in
            {id; textarea}

      let change_input msg = 
            Get.upd_textarea msg >>= fun textarea ->
            Set.textarea textarea

      let update = function
            | Init id -> init id
            | SaveItem -> save_item
            | Input msg -> change_input msg 
end

