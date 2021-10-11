open Entities

module type ENTITY_API = sig
      type t
      val last_id: unit -> int
      val get_all: unit -> t list
      val add_or_replace: t -> unit
      val delete: t Id.t -> unit
end

module type T = sig
      module ItemApi: ENTITY_API with type t := Item.t
      module FilterApi: ENTITY_API with type t := Filter.t
end
