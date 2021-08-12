open Utils

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
      let after_len = len - pos - 1 in
      let before = String.sub str 0 pos in
      let curr =
            begin if pos < len
            then String.get str pos
            else ' ' end
            |> Char.escaped
      in
      let after =
            begin if pos < len
            then String.sub str (pos + 1) after_len
            else " " end
      in
      (before, curr, after)

let type_char typed {chr; text} =
      let chr, line = pos in
      let data =
            data
            |> String.split_on_char '\n'
            |> List.mapi (fun index str ->
                  if index = line
                  then
                        let (before, curr, after) = split_on_pos chr str in
                        Printf.sprintf "%s%c%s%s" before typed curr after
                  else str
            )
            |> String.concat "\n"
      in
      {pos; data}
      |> if typed = '\n' then shift_cursor (-999, 1) else shift_cursor (1, 0)

let del_char {pos; data} =
      let chr, line = pos in
      let lines = String.split_on_char '\n' data in
      let rem_chr =
            List.mapi (fun index str ->
                  if index = line
                  then
                        let (before, curr, after) = split_on_pos chr str in
                        let before =
                              if before = ""
                              then "" 
                              else String.(sub before 0 (length before - 1))
                        in
                        Printf.sprintf "%s%s%s" before curr after
                  else str
            )
            >> String.concat "\n"
      in
      let rem_line =
            List.fold_left2 (fun acc index curr ->
                match index with
                | 0 -> curr
                | index when index = line -> acc ^ curr
                | _ -> acc ^ "\n" ^ curr
            ) "" List.(init (length lines) (fun num -> num))
      in
      let data =
            lines
            |> if chr = 0 then rem_line else rem_chr
      in
      {pos; data}
      |> shift_cursor @@
            if chr = 0
            then (int_of_float infinity, -1)
            else (-1, int_of_float infinity)

