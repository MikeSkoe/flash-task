open Notty
open Utils
open For_ui

type style =
      | Normal
      | Secondary
      | Selected
      | Underline

let convert_style = function
      | Normal -> A.empty
      | Secondary -> A.(fg @@ gray 10)
      | Selected -> A.(bg white ++ fg black)
      | Underline -> A.(st underline)

let text = convert_style >> I.string

let editable cursor style str =
      let style = convert_style style in
      let (before, curr, after) = Textarea.split_on_pos cursor str in
      let before = I.string style before in
      let curr = I.string (convert_style Selected) curr in
      let after = I.string style after in
      I.(before <|> curr <|> after)

