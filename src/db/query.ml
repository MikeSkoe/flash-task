open Entities

module R = Caqti_request
module T = Caqti_type

module ItemQuery = struct
      type t = Item.t
      type tup = int * string * string

      let t_of_tup (id, title, body) = Item.make ~id ~title ~body ()
      let tup_of_t (t: Item.t) = (t.id, t.title, t.body)

      let last_id = R.find
            T.unit
            T.int
            " SELECT max(id) from items "

      let create_table = R.exec
            T.unit
            "
                  CREATE TABLE IF NOT EXISTS items (
                        id INTEGER PRIMARY KEY,
                        title TEXT,
                        body TEXT
                  )
            "

      let get_all = R.collect
            T.unit
            T.(tup3 int string string)
            "
                  SELECT id, title, body
                  FROM items
            "

      let get_via_filter (filter: Filter.t) =
          let tags = match filter.rule with
              | Filter.All -> []
              | Filter.WithTags tags -> tags
          in
          let str = match tags with
              | [] -> ""
              | tags ->
                  tags
                  |> List.map (fun tag -> Printf.sprintf "title LIKE \'%%%s%%\'" Tag.(as_string tag))
                  |> String.concat " OR "
                  |> (^) " WHERE "
          in
          R.collect
            T.unit
            T.(tup3 int string string)
            ("
                  SELECT id, title, body
                  FROM items
            " ^ str)

      let add_or_replace = R.exec
            T.(tup3 int string string)
            "
                  REPLACE INTO items (id, title, body)
                  VALUES (?, ?, ?)
            "

      let delete = R.exec
            T.int
            "
                  DELETE FROM items
                  WHERE id = ?
            "
end

module FilterQuery = struct
      type t = Filter.t
      type tup = int * string

      let t_of_tup (id, title) = Filter.make ~id ~title ()
      let tup_of_t (t: Filter.t) = (t.id, t.title)

      let last_id = R.find
            T.unit
            T.int
            " SELECT max(id) from filters "

      let create_table = R.exec
            T.unit
            "
                  CREATE TABLE IF NOT EXISTS filters (
                        id INTEGER PRIMARY KEY,
                        title TEXT
                  )
            "

      let get_all = R.collect
            T.unit
            T.(tup2 int string)
            "
                  SELECT id, title
                  FROM filters
            "


      let add_or_replace = R.exec
            T.(tup2 int string)
            "
                  REPLACE INTO filters (id, title)
                  VALUES (?, ?)
            "

      let delete = R.exec
            T.int
            "
                  DELETE FROM filters
                  WHERE id = ?
            "
end

