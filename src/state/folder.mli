type t

val empty : t
val add_item : Item.t -> t -> t
val get_items : t -> Item.t list

(* --- TEST --- *)
val sample : t

