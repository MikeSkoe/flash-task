open Utils

let memo ?is_equal:((=)=(=)) fn =
      let last_arg_res = ref None in
      (fun arg -> match !last_arg_res with
      | Some (last_arg, last_res) when last_arg = arg ->
            last_res
      | _ ->
            let new_res = fn arg in
            last_arg_res := Some (arg, new_res);
            new_res
      )

type ('a, 'b) t = Select of ('a -> 'b)

let apply (Select fn) = fn

let make ?is_equal:(is_equal=(=)) fn =
      Select (memo ~is_equal fn)
let map ?is_equal:(is_equal=(=)) (Select fn) map_fn =
      Select (memo ~is_equal (fn >> map_fn))
let map2 ?is_equal:(is_equal=(=)) (Select fn1) (Select fn2) map_fn =
      Select (memo ~is_equal (fun arg ->
            let a = fn1 arg in
            let b = fn2 arg in
            map_fn a b)
      )
let if_map ?is_equal:(is_equal=(=)) (Select check) (Select on_true) (Select on_false) =
      Select (memo ~is_equal (fun arg ->
            if check arg
            then on_true arg
            else on_false arg
      ))

