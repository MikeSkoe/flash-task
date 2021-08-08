(* TODO:? rewrite to functor with current line as inner value *)
type t = {
      pos: int * int;
      data: string;
}

let empty = {
      pos=(2, 0);
      data="Empty data";
}

let normalize {pos; data} =
      let chr, line = pos in
      let strings = String.split_on_char '\n' data in
      let height = List.(length strings) - 1 in
      let line =
            line
            |> max 0
            |> min height
      in
      let curr_string = List.nth strings line in
      let width = String.length curr_string in
      let chr =
            chr
            |> max 0
            |> min width
      in
      {pos=(chr, line); data}

let shift_cursor (shift_x, shift_y) t =
      let (pos_x, pos_y) = t.pos in
      {
            t with
            pos=pos_x + shift_x, pos_y + shift_y
      }
      |> normalize

let split_on_pos pos str =
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

let type_char typed {pos; data} =
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
      |> shift_cursor (1, 0)

let del_char {pos; data} =
      let chr, line = pos in
      let data =
            data
            |> String.split_on_char '\n'
            |> List.mapi (fun index str ->
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
            |> String.concat "\n"
      in
      {pos; data}
      |> shift_cursor (-1, 0)

