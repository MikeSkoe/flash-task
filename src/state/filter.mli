type rule =
    | WithTag of Tag.t
type t

val make : string -> rule list -> t
val empty : t

val get_name : t -> string

val filter : t -> Item.t list -> Item.t list

