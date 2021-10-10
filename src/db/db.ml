open Entities
open Utils

let connection_url = Printf.sprintf "sqlite3://%s/items.db" Sys.(getcwd ())

let pool = Caqti_blocking.connect_pool (Uri.of_string connection_url)
      |> Caqti_blocking.or_fail

let use_or_fail exp =
      Caqti_blocking.Pool.use exp pool
      |> Caqti_blocking.or_fail

module type API = sig
      type t

      val create_table: unit -> unit
      val get_all: unit -> t list
      val add_or_replace: t -> unit
      val delete: int -> unit
end

module Api = struct
      (* generate Iterm and Filter Api's from module functor *)
      module ItemApi: API with type t = Item.t = struct
            module Q = Query.ItemQuery

            type t = Item.t

            let create_table () =
                  (fun (module CON: Caqti_blocking.CONNECTION) ->
                        CON.exec Q.create_table ()
                  )
                  |> use_or_fail

            let get_all () =
                  let item_of_tup3 (id, title, body) = Item.make ~id:id title body in
                  (fun (module CON: Caqti_blocking.CONNECTION) ->
                        CON.fold Q.get_all (item_of_tup3 >> List.cons) () []
                  )
                  |> use_or_fail

            let add_or_replace (item: t) =
                  (fun (module CON: Caqti_blocking.CONNECTION) ->
                        CON.exec Q.add_or_replace (item.id, item.title, item.body)
                  )
                  |> use_or_fail
            
            let delete id =
                  (fun (module CON: Caqti_blocking.CONNECTION) ->
                        CON.exec Q.delete id
                  )
                  |> use_or_fail
      end

      module FilterApi: API with type t = Filter.t = struct
            module Q = Query.FilterQuery

            type t = Filter.t

            let create_table () =
                  (fun (module CON: Caqti_blocking.CONNECTION) ->
                        CON.exec Q.create_table ()
                  )
                  |> use_or_fail
            
            let get_all () =
                  let filter_of_tup2 (id, title) = Filter.make ~id:id title in
                  (fun (module CON: Caqti_blocking.CONNECTION) ->
                        CON.fold Q.get_all (filter_of_tup2 >> List.cons) () []
                  )
                  |> use_or_fail

            let add_or_replace (filter: t) =
                  (fun (module CON: Caqti_blocking.CONNECTION) ->
                        CON.exec Q.add_or_replace (filter.id, filter.title)
                  )
                  |> use_or_fail
            
            let delete id =
                  (fun (module CON: Caqti_blocking.CONNECTION) ->
                        CON.exec Q.delete id
                  )
                  |> use_or_fail
      end
end
