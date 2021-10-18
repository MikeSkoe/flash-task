open Utils

(* TODO:? rewrite to functor with current line as inner value *)
type t = {
      chr: int;
      text: string;
}

type msg =
      | ShiftCursor of int
      | TypeChar of char
      | DelChar

let empty = {
      chr=0;
      text="";
}

let make text = { empty with text }

module Get = struct
      let chr {chr; text} =
            let width = String.length text in
            chr |> max 0 |> min (width - 1)

      let split_on_pos {chr; text} =
            let len = String.length text in
            let after_len = len - chr - 1 in
            let before = String.sub text 0 chr in
            let curr =
                  Char.escaped @@
                  if chr < len
                  then String.get text chr
                  else ' '
            in
            let after =
                  begin if chr < len
                  then String.sub text (chr + 1) after_len
                  else " " end
            in
            (before, curr, after)
end

module Set = struct
      let text text t = {t with text}
      let chr chr t = {t with chr}
end

let (>>=) = Select.(>>=)

let shift_cursor shift_x =
      Get.chr >>= fun chr ->
      Set.chr (chr + shift_x)

let type_char typed =
      Get.split_on_pos >>= fun (before, curr, after) ->

      let text = Printf.sprintf "%s%c%s%s" before typed curr after in
      let shift =
            if typed = '\n' 
            then (-(int_of_float infinity))
            else 1
      in

      Set.text text >> shift_cursor shift

let del_char =
      Get.chr >>= fun chr ->
      Get.split_on_pos >>= fun (before, curr, after) ->

      let before =
            if before = ""
            then "" 
            else String.(sub before 0 (length before - 1))
      in
      let text = Printf.sprintf "%s%s%s" before curr after in
      let shift =
            if chr = 0
            then int_of_float infinity
            else -1
      in

      Set.text text >> shift_cursor shift

let update = function
      | ShiftCursor shift_x -> shift_cursor shift_x
      | TypeChar chr -> type_char chr
      | DelChar -> del_char

