let memo ?is_equal:(is_eq=(=)) fn =
  let last_arg_res = ref None in
  (fun arg ->
    match !last_arg_res with
    | Some (last_arg, last_res) when is_eq last_arg arg -> last_res
    | _ ->
      let new_res = fn arg in
      last_arg_res := Some (arg, new_res);
      new_res
  )

(*
 * type st
 * type arg
 * type st_arg = st * arg // st * 'a 
 *
 * type setter: arg -> st * 'a -> st
 * type getter: st * 'a -> st
 * 
 * let get_arg = st * 'a -> 'a
 *
 * let set_st_ii =
 *     get_arg >>= fun shift ->
 *     get_ii >>= fun ii ->
 *     get_items >>= fun items ->
 *     let ii =
 *         (ii + shift)
 *         |> max 0
 *         |> min List.(length items - 1)
 *     in
 *     set_ii ii;;
 *)

type ('a, 'b) t = ('a -> 'b)
  
let return res = (fun _ -> res)

let id = (fun a -> a)

let get_arg (_, arg) = arg

let (>>=) fn bind_fn =
      let memo_bind = memo bind_fn in
      fun arg ->
            let new_val = fn arg in
            let fn = memo_bind new_val in
            fn arg
 
let branch check if_true if_false =
      fun arg -> if check arg then if_true arg else if_false arg

