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

(* --- TEST --- *)
    
      (*
let strings_1 = ["first title"; "first description"; "first tag\nanother tag\nlearn"]
let strings_2 = ["second title"; "second description"; "single tags"]
let strings_3 = ["third title"; "third description"; ""]

let item_1 = Item.make "first title" List.(["first tag";"another tag";"learn"] |> map Tag.make) "first description"
let item_2 = Item.make "second title" List.(["single tags"] |> map Tag.make) "second description"
let item_3 = Item.make "third title" [] "third description"

let%test "\n--- [PARSER] strings to items\n" =
      let items =
            [strings_1;strings_2;strings_3]
            |> List.map item_of_strings
      in
      let expected = [item_1;item_2;item_3] in
      items = expected

let%test "\n--- [PARSER] items to strings\n" =
      let items =
            [item_1;item_2;item_3]
            |> List.map strings_of_item 
      in
      let expected = [strings_1;strings_2;strings_3] in
      items = expected
      *)
