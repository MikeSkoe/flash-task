let (>>) f1 f2 arg = f2 (f1 arg)
let rev f a1 a2 = f a2 a1

let intersect (a: 'a list) =
      let mem = rev List.mem @@ a in
      List.exists mem

let rec find ?eq:((=)=(=))x lst =
      match lst with
      | [] -> raise (Failure "Not Found")
      | h :: t -> if x = h then 0 else 1 + find ~eq:(=) x t

(* --- TEST --- *)

let%test "\n--- [LIST] check two lists share common items" =
      let list_a = [1;2;3] in
      let list_b = [2;4;6] in
      let list_c = [6;7;8] in
      intersect list_a list_b &&
      intersect list_b list_a &&
      intersect list_b list_c &&
      intersect list_c list_b &&
      not @@ intersect list_c list_a &&
      not @@ intersect list_a list_c

