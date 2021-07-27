open Utils
open State

let item_of_arr arr = 
      let make acc data =
            match data with
            | title :: description :: tags :: _ ->
                  let tags =
                      (match Csv.input_all @@ Csv.of_string tags with
                      | [tags] -> tags
                      | _ -> []
                      )
                  in
                  let item = Item.make title tags description in
                  item :: acc
            | title :: description :: _ ->
                  let item = Item.make title [] description in
                  item :: acc
            | title :: _ ->
                  let item = Item.make title [] "" in
                  item :: acc
            | _ -> acc
      in
      List.fold_left make [] arr

let item_of_scv_string str =
      str
      |> Csv.of_string
      |> Csv.input_all
      |> List.rev
      |> item_of_arr

let csv_string_of_item item =
    let title = Item.get_title item in
    let body = Item.get_body item in
    let tags =
        Item.get_tags item
        |> String.concat ","
    in
    Printf.sprintf {|"%s","%s","%s"|} title body tags

let csv_string_of_items =
    List.(map csv_string_of_item)
    >> String.(concat "\n")


(* --- TEST --- *)

let sample =
{|"first title","first description","first tag, another tag, learn"
second title,second description,single tags
third title,third description,|}

let sample_quoted =
{|"first title","first description","first tag,another tag,learn"
"second title","second description","single tags"
"third title","third description",""|}

let%test "\n--- [PARSER] check CSV parsing\n" =
      let parsed =
            sample
            |> Csv.of_string
            |> Csv.input_all
      in 
      let expected = [
            ["first title"; "first description"; "first tag, another tag, learn"];
            ["second title"; "second description"; "single tags"];
            ["third title"; "third description"; ""];
      ]
      in
      parsed = expected

let%test "\n--- [PARSER] item of csv string\n" =
      let items = item_of_scv_string sample in
      let expected = [
            Item.make "first title" ["first tag";"another tag";"learn"] "first description";
            Item.make "second title" ["single tags"] "second description";
            Item.make "third title" [] "third description";
      ]
      in
      items = expected

let%test "\n--- [PARSER] csv string of single item\n" =
    let expected = {|"first title","first description","first tag,another tag,learn"|} in
    let str = csv_string_of_item @@ Item.make "first title" ["first tag";"another tag";"learn"] "first description" in
    str = expected

let%test "\n--- [PARSER] csv string of item list\n" =
    let str = csv_string_of_items  [
            Item.make "first title" ["first tag";"another tag";"learn"] "first description";
            Item.make "second title" ["single tags"] "second description";
            Item.make "third title" [] "third description";
    ] in
    str = sample_quoted

