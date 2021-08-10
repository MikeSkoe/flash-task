type t

val make : string -> Tag.t list -> string -> t
val empty : t

val get_tags : t -> Tag.t list
val get_title : t -> string
val get_body : t -> string

val has_tag : Tag.t -> t -> bool
val eq : t -> t -> bool

