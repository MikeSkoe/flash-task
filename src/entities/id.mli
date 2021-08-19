type 'a t
val of_string : String.t -> 'a t
val get_next : unit -> 'a t
val get_same : unit -> 'a t
val print: 'a t -> unit

