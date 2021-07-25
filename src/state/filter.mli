type t
val empty : t
val add_tag : Tag.t Id.t -> Item.t list -> t -> t
val remove_tag : Tag.t Id.t -> Item.t list -> t -> t
val update_items : Item.t list -> t -> t
val next : t -> t
val prev : t -> t
val get_selected : t -> Item.t Id.t option

