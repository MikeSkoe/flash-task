type 'a t

val push: 'a -> 'a t -> 'a t
val back: 'a t -> 'a t
val shift: 'a -> 'a t -> 'a t
val make: 'a -> 'a t

