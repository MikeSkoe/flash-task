module R = Caqti_request
module T = Caqti_type

module ItemQuery = struct
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
