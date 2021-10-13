open Utils

let connection_url = Printf.sprintf "sqlite3://%s/items.db" Sys.(getcwd ())

let pool = Caqti_blocking.connect_pool (Uri.of_string connection_url)
      |> Caqti_blocking.or_fail

let use_or_fail exp =
      Caqti_blocking.Pool.use exp pool
      |> Caqti_blocking.or_fail

module type BASE_API = sig
      type t
      type tup

      val last_id: unit -> int
      val create_table: unit -> unit
      val get_all: unit -> t list
      val add_or_replace: t -> t list
      val delete: int -> unit
end

module type BASE_API_QUERY = sig
      type t
      type tup

      val tup_of_t: t -> tup
      val t_of_tup: tup -> t

      val last_id : (unit, int, [< `Many | `One | `Zero > `One ]) Caqti_request.t
      val create_table : (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
      val get_all : (unit, tup, [ `Many | `One | `Zero ]) Caqti_request.t
      val add_or_replace : (tup, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
      val delete : (int, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
end

module BaseApi (Q: BASE_API_QUERY): BASE_API with type t := Q.t and type tup := Q.tup = struct
      let create_table () =
            (fun (module CON: Caqti_blocking.CONNECTION) ->
                  CON.exec Q.create_table ()
            )
            |> use_or_fail

      let last_id () =
            try (fun (module CON: Caqti_blocking.CONNECTION) -> CON.find Q.last_id ())
                |> use_or_fail
            with Caqti_error.Exn _ -> 1

      let get_all () =
            (fun (module CON: Caqti_blocking.CONNECTION) ->
                  CON.fold Q.get_all (Q.t_of_tup >> List.cons) () []
            )
            |> use_or_fail

      let add_or_replace t =
            (fun (module CON: Caqti_blocking.CONNECTION) ->
                  let _ = CON.exec Q.add_or_replace Q.(tup_of_t t) in
                  CON.fold Q.get_all (Q.t_of_tup >> List.cons) () []
            )
            |> use_or_fail
      
      let delete id =
            (fun (module CON: Caqti_blocking.CONNECTION) ->
                  CON.exec Q.delete id
            )
            |> use_or_fail
end

module ItemApi = struct
      include BaseApi (Query.ItemQuery)

      let get_via_filter filter =
            (fun (module CON: Caqti_blocking.CONNECTION) ->
                  CON.fold Query.ItemQuery.(get_via_filter filter) Query.ItemQuery.(t_of_tup >> List.cons) () []
            )
            |> use_or_fail
end

module FilterApi = struct
      include BaseApi (Query.FilterQuery)
end

