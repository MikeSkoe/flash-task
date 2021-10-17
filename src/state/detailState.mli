type t

val empty : t

type msg = Init of int | Input of For_ui.Textarea.msg | SaveItem

module Get : sig
  val id : t -> int
  val item : t -> Entities.Item.t
  val upd_textarea : For_ui.Textarea.msg -> t -> For_ui.Textarea.t
  val textarea : t -> For_ui.Textarea.t
end

module Set : sig
  val textarea : For_ui.Textarea.t -> t -> t
end

module Make : functor (Api : Api_type.T) -> sig
  val update : msg -> t -> t
end
