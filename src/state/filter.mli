type rule_item =
    | WithTag of Tag.t

type rule = 
    | WithoutTags
    | All
    | OptTag of rule_item list

type t

val make : string -> rule -> t
val empty : t

val get_title : t -> string
val get_rule : t -> rule

val filter : t -> Item.t list -> Item.t list

