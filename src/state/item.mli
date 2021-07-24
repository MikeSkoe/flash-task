type t

val make : string -> string list -> string -> t
val get_id : t -> t Id.t
val has_tag : Tag.t Id.t -> t -> bool
val get_tags : t -> string list
val compare : t -> t -> int

(* --- TEST --- *)

val sample_1 : t
val sample_2 : t
