  type ('a, 'b) t
  
  val make: ('a -> 'b) -> ('a, 'b) t
  val return: 'b -> ('a, 'b) t
  val apply: ('a, 'b) t -> 'a -> 'b
  val (>>=): ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
  val branch: ('a, bool) t -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  
