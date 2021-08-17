type ('a, 'b) t

val apply:
      ('a, 'b) t
      -> ('a -> 'b)

val make: ?is_equal:('a -> 'a -> bool)
      -> ('a -> 'b)
      -> ('a, 'b) t
val map: ?is_equal:('a -> 'a -> bool)
      -> ('a, 'b) t
      -> ('b -> 'c)
      -> ('a, 'c) t
val map2: ?is_equal:('a -> 'a -> bool)
      -> ('a, 'b) t
      -> ('a, 'c) t
      -> ('b -> 'c -> 'd)
      -> ('a, 'd) t
val if_map: ?is_equal:('a -> 'a -> bool)
      -> ('a, bool) t
      -> ('a, 'b) t
      -> ('a, 'b) t
      -> ('a, 'b) t

