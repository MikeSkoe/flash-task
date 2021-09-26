open Utils

let item_of_strings = function
      | title :: description :: tags :: _ ->
            let tags = tags |> String.split_on_char '\n'in
            Item.make title List.(map Tag.make tags) description
      | title :: description :: _ ->
            Item.make title [] description
      | title :: _ ->
            Item.make title [] ""
      | _ ->
            Item.empty

let strings_of_item item =
      let title = Item.get_title item in
      let body = Item.get_body item in
      let tags =
            Item.get_tags item
            |> List.map Tag.get_title
            |> String.concat "\n"
      in
      [title;body;tags]

let item_of_string str = match String.split_on_char '\n' str with
      | [] ->
            Item.empty
      | title :: [] ->
            let title = String.trim title in
            Item.make title [] ""
      | title :: tags :: [] ->
            let title = String.trim title in
            let tags = tags
                  |> String.split_on_char ','
                  |> List.map (String.trim >> Tag.make)
            in
            Item.make title tags ""
      | title :: tags :: body ->
            let title = String.trim title in
            let tags = tags
                  |> String.split_on_char ','
                  |> List.map (String.trim >> Tag.make)
            in
            let body = String.(concat "\n" body) in
            Item.make title tags body

let string_of_item item =
      let title = Item.get_title item in
      let body = Item.get_body item in
      let tags =
            Item.get_tags item
            |> List.map Tag.get_title
            |> String.concat ","
      in
      Printf.sprintf "%s\n%s\n%s" title tags body

let string_of_rule = function
      | Filter.All -> "*"
      | Filter.WithTags tags ->
            tags
            |> List.map (fun tag -> Printf.sprintf "+%s" Tag.(get_title tag))
            |> String.concat ","

let rule_of_string = function
      | "*" -> Filter.All
      | tags -> Filter.WithTags (
            tags
            |> String.split_on_char ','
            |> List.map (
                  String.to_seq
                  >> List.of_seq
                  >> (function
                     | '+' :: tail ->
                           let tail = List.to_seq tail |> String.of_seq in
                           Tag.(make tail)
                     | _ -> Tag.(make ""))
            )
      )

let string_of_filter filter =
    let title = Filter.Get.title filter in
    let rule =
          Filter.Get.rule filter
          |> string_of_rule 
    in
    String.concat "\n" [title; rule]

let filter_of_string str = match String.split_on_char '\n' str with
      | [] -> Filter.empty
      | title :: [] -> Filter.(make title All)
      | title :: rule :: _ ->
            let rule = rule_of_string rule in
            Filter.make title rule

let of_file filename =
      let items =
            Csv.load filename
            |> List.map item_of_strings
      in
      let filters = [
            Filter.empty;
            Filter.(make "---filter #tag" (WithTags [Tag.(make "tag")]));
            Filter.(make "---filter #tag3" (WithTags [Tag.(make "tag3")]));
      ]
      in
      items, filters

let to_file filename items =
      items
      |> List.map strings_of_item
      |> Csv.save filename

