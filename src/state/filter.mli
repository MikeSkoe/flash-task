type rule =
    | WithTag of Tag.t Id.t
type t

val empty : t
val make : string -> rule list -> t

val filter : t -> Item.t list -> Item.t list

