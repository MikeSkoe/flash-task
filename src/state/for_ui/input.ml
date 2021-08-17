(* TODO:? rewrite to functor with current line as inner value *)
type t = {
      chr: int;
      text: string;
}

let empty = {
      chr=0;
      text="";
}

let make text = { empty with text }

let normalize {chr; text} =
      let width = String.length text in
      let chr =
            chr
            |> max 0
            |> min width
      in
      {chr; text}

let shift_cursor shift_x t =
      { t with chr=t.chr + shift_x }
      |> normalize

let split_on_pos chr str =
      let len = String.length str in
      let after_len = len - chr - 1 in
      let before = String.sub str 0 chr in
      let curr =
            begin if chr < len
            then String.get str chr
            else ' ' end
            |> Char.escaped
      in
      let after =
            begin if chr < len
            then String.sub str (chr + 1) after_len
            else " " end
      in
      (before, curr, after)

let type_char typed {chr; text} =
      let text =
            let (before, curr, after) = split_on_pos chr text in
            Printf.sprintf "%s%c%s%s" before typed curr after
      in
      {chr; text}
      |> if typed = '\n' then shift_cursor (-(int_of_float infinity)) else shift_cursor 1

let del_char {chr; text} =
      let text = 
            let (before, curr, after) = split_on_pos chr text in
            let before =
                  if before = ""
                  then "" 
                  else String.(sub before 0 (length before - 1))
            in
            Printf.sprintf "%s%s%s" before curr after
      in
      {chr; text}
      |> shift_cursor @@
            if chr = 0
            then int_of_float infinity
            else -1

