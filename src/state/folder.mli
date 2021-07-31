type t

val empty : t
val make : Item.t list -> Filter.t list -> t

val get_items : t -> Filter.t -> Item.t list
val get_filters : t -> Filter.t list
val get_selected : t -> Item.t option

val add_items : Item.t list -> t -> t
val add_filters : Filter.t list -> t -> t

val next_selected : t -> t
val prev_selected : t -> t

(* --- TEST --- *)

