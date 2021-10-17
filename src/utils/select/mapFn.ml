type ('a, 'b) t = ('a -> 'b)
  
let return res = (fun _ -> res)

let id = (fun a -> a)

let get_arg (_, arg) = arg

let (>>=) fn bind_fn =
      fun arg ->
            let new_val = fn arg in
            let fn = bind_fn new_val in
            fn arg
 
