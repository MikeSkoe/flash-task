type t = private {
      id: t Id.t;
      title: string;
      tags: Tag.t list;
      body: string;
}

module Get: sig
    val title: t -> string
end

module Set: sig
    val id: t Id.t -> t -> t
end

val make: ?id:(t Id.t) -> title:string -> body:string -> unit -> t

val empty: t

(* Checkers *)
val has_tag: Tag.t -> t -> bool

val has_no_tag: t -> bool

val eq: t -> t -> bool

