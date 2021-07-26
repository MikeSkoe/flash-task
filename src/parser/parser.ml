open State

let item_of_arr arr = 
      let make acc data =
            match data with
            | title :: description :: tags :: _ ->
                  let _ = print_endline @@ Printf.sprintf "%s|%s|%s" title description tags in
                  let item = Item.make_with_id title [tags] description in
                  item :: acc
            | title :: description :: _ ->
                  let item = Item.make_with_id title [] description in
                  item :: acc
            | title :: _ ->
                  let item = Item.make_with_id title [] "" in
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

(* --- TEST --- *)

let sample =
{|"first title","first description","first tag, another tag, learn"
second title,second description,single tags|}

let%test "\n--- [PARSER] check CSV parsing\n" =
      let parsed =
            sample
            |> Csv.of_string
            |> Csv.input_all
      in 
      let expected = [
            ["first title"; "first description"; "first tag, another tag, learn"];
            ["second title"; "second description"; "single tags"];
      ]
      in
      parsed = expected

let%test "\n--- [PARSER] item of csv string\n" =
      let items = item_of_scv_string sample in
      let expected = [
            Item.make_with_id "first title" ["first tag, another tag, learn"] "first description";
            Item.make_with_id "second title" ["single tags"] "second description";
      ]
      in
      items = expected

