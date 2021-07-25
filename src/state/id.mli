type 'a t
val of_string : String.t -> 'a t
val get_next : unit -> 'a t
val compare : 'a t -> 'a t -> int
val print: 'a t -> unit
