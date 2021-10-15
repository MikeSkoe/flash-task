type t

val empty : t

type msg =
      | Init
      | ShiftSelected of int * int
      | DeleteItem
      | DeleteFilter
      | AddItem of string
      | AddFilter of string
      | Input of For_ui.Input.msg

module Get : sig
      val return : 'a -> 'b -> 'a
      val items : t -> Entities.Item.t list
      val filters : t -> Entities.Filter.t list
      val selected : t -> Entities.Selected.t
      val input : t -> For_ui.Input.t

      val items_len : t -> int
      val filters_len : t -> int
      val fi : t -> int
      val ii : t -> int
      val cur_filter : t -> Entities.Filter.t
      val cur_item : t -> Entities.Item.t
end

module Make : functor (Api : Api_type.T) -> sig
      val update : msg -> t -> t
end
