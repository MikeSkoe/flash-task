let (>>) f1 f2 arg = f2 (f1 arg)
let flip f a1 a2 = f a2 a1

let intersect (a: 'a list) =
      let mem = flip List.mem @@ a in
      List.exists mem

let rec find ?eq:((=)=(=))x lst =
      match lst with
      | [] -> raise (Failure "Not Found")
      | h :: t -> if x = h then 0 else 1 + find ~eq:(=) x t

