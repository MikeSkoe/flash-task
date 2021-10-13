let item_of_tup3 (id, title, body) = Item.make ~id ~title ~body ()

let filter_of_tup2 (id, title) = Filter.make ~id ~title ()

let item_of_string id str = match String.split_on_char '\n' str with
      | [] ->
            Item.empty
      | title :: [] ->
            let title = String.trim title in
            Item.make ~id ~title ~body:"" ()
      | title :: body ->
            let title = String.trim title in
            let body = String.(concat "\n" body) in
            Item.make ~id ~title ~body ()

let string_of_item (item: Item.t) = Printf.sprintf "%s\n%s" item.title item.body

let string_of_rule = function
      | Filter.All -> "*"
      | Filter.WithTags tags ->
            tags
            |> List.map Tag.as_string
            |> String.concat ","

let string_of_filter filter =
    let title = Filter.Get.title filter in
    let rule =
          Filter.Get.rule filter
          |> string_of_rule 
    in
    String.concat "\n" [title; rule]

let filter_of_string id title = Filter.make ~id ~title ()

let of_file filename =
      let items =
            Csv.load filename
            |> List.map (function
                  | title :: body :: _ -> Item.make ~title ~body ()
                  | title ::  _ -> Item.make ~title ~body:"" ()
                  |  _ -> Item.empty
            )
      in
      let filters = [ ] in
      items, filters

let to_file filename items =
      items
      |> List.map (fun item -> [string_of_item item])
      |> Csv.save filename

