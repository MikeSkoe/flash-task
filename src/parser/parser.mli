open State

val item_of_arr : string list list -> Item.t list
val item_of_scv_string : string -> Item.t list
val csv_string_of_item : Item.t -> string
val csv_string_of_items : Item.t list -> string

