let memo ?eq:(eq=(=)) fn =
    let last_arg_res = ref None in
    (fun arg ->
        match !last_arg_res with
        | Some (last_arg, last_res) when eq last_arg arg -> last_res
        | _ ->
            let new_res = fn arg in
            last_arg_res := Some (arg, new_res);
            new_res
    )

type ('a, 'b) t = ('a -> 'b)
  
let return res = (fun _ -> res)

let id a = a

(* TODO: try applicative approach *)
let bind ?map:(map_bind_fn=id) fn bind_fn =
      let bind_fn = map_bind_fn bind_fn in
      fun arg ->
            let new_val = fn arg in
            let fn = bind_fn new_val in
            fn arg

let (>>=) fn bind_fn = bind ~map:id fn bind_fn
 
