open Entities

(* CSV conversions *)
val item_of_strings : string list -> Item.t
val strings_of_item : Item.t -> string list

(* UI conversions *)
val item_of_string : string -> Item.t
val string_of_item : Item.t -> string

val filter_of_string : string -> Filter.t
val string_of_filter : Filter.t -> string

val rule_of_string : string -> Filter.rule
val string_of_rule : Filter.rule -> string

val of_file : string -> Folder.t
val to_file : string -> Folder.t -> unit

