type t = private string

val as_string: t -> string
val is_tag: string -> bool
val make: string -> t
val tags_of_string: string -> t list
