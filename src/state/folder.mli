type t

val empty : t
val add_item : t -> Item.t -> t
val get_items : t -> Item.t list
val get_filter : t -> Filter.t

val next_selected : t -> t
val prev_selected : t -> t

(* --- TEST --- *)
val sample : t

