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
      | Filter.WithoutTags -> "-"
      | Filter.OptTag rule_items ->
            rule_items
            |> List.map (fun (Filter.WithTag tag) -> Printf.sprintf "+%s" Tag.(get_title tag))
            |> String.concat ","

let rule_of_string = function
      | "*" -> Filter.All
      | "-" -> Filter.WithoutTags
      | rule_items -> 
            rule_items
            |> String.split_on_char ','
            |> List.map (
                  String.to_seq
                  >> List.of_seq
                  >> (function
                     | '+' :: tail ->
                           let tail = List.to_seq tail |> String.of_seq in
                           Filter.WithTag Tag.(make tail)
                     | _ -> Filter.WithTag Tag.(make ""))
            )
            |> (fun rule_items -> Filter.OptTag rule_items)

let string_of_filter filter =
    let title = Filter.get_title filter in
    let rule =
          Filter.get_rule filter
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
      Folder.(add_items items empty
            |> add_filters @@ [
                  Filter.(make "---filter #tag" (OptTag [WithTag Tag.(make "tag")]));
                  Filter.(make "---filter #tag3" (OptTag [WithTag Tag.(make "tag3")]));
            ])

let to_file filename folder =
      Folder.get_items folder
      |> List.map strings_of_item
      |> Csv.save filename

