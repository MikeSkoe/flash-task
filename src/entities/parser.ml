let item_of_tup3 (id, title, body) = Item.make ~id:id title body

let filter_of_tup2 (id, title) = Filter.make ~id:id title

let item_of_string str = match String.split_on_char '\n' str with
      | [] ->
            Item.empty
      | title :: [] ->
            let title = String.trim title in
            Item.make title ""
      | title :: body ->
            let title = String.trim title in
            let body = String.(concat "\n" body) in
            Item.make title body

let string_of_item item =
      let title = Item.Get.title item in
      let body = Item.Get.body item in
      Printf.sprintf "%s\n%s" title body

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

let filter_of_string str = Filter.make str

let of_file filename =
      let items =
            Csv.load filename
            |> List.map (function
                  | title :: body :: _ -> Item.make title body
                  | title ::  _ -> Item.make title ""
                  |  _ -> Item.make "" ""
            )
      in
      let filters = [ ] in
      items, filters

let to_file filename items =
      items
      |> List.map (fun item -> [string_of_item item])
      |> Csv.save filename

