type t

val empty : t
val make : Item.t list -> Filter.t list -> t

val get_items : t -> Filter.t -> Item.t list
val get_filters : t -> Filter.t list
val get_selected : t -> (Filter.t * Item.t)

val add_items : Item.t list -> t -> t
val add_filters : Filter.t list -> t -> t

val delete_items : Item.t list -> t -> t
val delete_filters : Filter.t list -> t -> t

val next_filter : t -> t
val prev_filter : t -> t
val next_item : t -> t
val prev_item : t -> t

(* --- TEST --- *)

