type t

val empty : t
val make : Item.t list -> Filter.t list -> t

val get_items : t -> Item.t list
val get_filters : t -> Filter.t list
val get_selected : t -> Selected.t

val add_items : Item.t list -> t -> t
val add_filters : Filter.t list -> t -> t

val delete_items : Item.t list -> t -> t
val delete_filters : Filter.t list -> t -> t

val shift_filter : int -> t -> t
val shift_item : int -> t -> t

(* --- TEST --- *)

