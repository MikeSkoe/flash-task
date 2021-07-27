let item_of_strings = function
      | title :: description :: tags :: _ ->
            let tags =
                (match Csv.input_all @@ Csv.of_string tags with
                | [tags] -> tags
                | _ -> []
                )
            in
            Item.make title tags description
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
        |> String.concat ","
    in
    [title;body;tags]

