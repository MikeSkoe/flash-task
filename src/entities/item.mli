type t

val make : string -> Tag.t list -> string -> t
val empty : t

val get_tags : t -> Tag.t list
val get_title : t -> string
val get_body : t -> string
val get_id : t -> t Id.t

val set_id : t Id.t -> t -> t
val set_tags : Tag.t list -> t -> t

val has_tag : Tag.t -> t -> bool
val has_no_tag : t -> bool
val eq : t -> t -> bool

