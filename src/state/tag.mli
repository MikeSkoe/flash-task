type t

val make : string -> t
val get_id : t -> t Id.t
val get_title : t -> string
val compare : t -> t -> int

(* --- TEST --- *)

val sample_1 : t
val sample_2 : t

