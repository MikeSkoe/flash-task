open Utils

type t = string

let is_tag =
      String.to_seq
      >> List.of_seq
      >> function
      | '#' :: _ ->  true
      | _ ->  false

let make t = t

let as_string t = t

let tags_of_string str =
      String.split_on_char ' ' str
      |> List.filter is_tag
      |> List.map make
