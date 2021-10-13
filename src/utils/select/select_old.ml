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

type ('a, 'b) t = Select of ('a -> 'b)
  
let return res = Select (fun _ -> res)

let id = (Select (fun a -> a))

let apply (Select fn) = fn

let (>>=) (Select fn) bind_fn =
      let memo_bind = memo bind_fn in
      Select (fun arg ->
            let new_val = fn arg in
            let (Select fn) = memo_bind new_val in
            fn arg
      )
 
let branch (Select check) (Select if_true) (Select if_false) =
      Select (fun arg -> if check arg then if_true arg else if_false arg)

