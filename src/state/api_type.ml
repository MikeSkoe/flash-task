open Entities

module type Api = sig
      type t
      val get_all: unit -> t list
      val add_or_replace: t -> unit
      val delete: t Id.t -> unit
end

module type T = sig
      module ItemApi: Api with type t := Item.t
      module FilterApi: Api with type t := Filter.t
end