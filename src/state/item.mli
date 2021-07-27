type t

val make : string -> string list -> string -> t

val get_id : t -> t Id.t
val get_tags : t -> string list
val get_title : t -> string
val get_body : t -> string
val has_tag : Tag.t Id.t -> t -> bool
val compare : t -> t -> int

(* --- TEST --- *)

val sample_1 : t
val sample_2 : t
