open Utils

type 'a index_type = Index of int

type t = Filter.t index_type * Item.t index_type

let empty = Index 0, Index 0

let make (fi, ii) = (Index fi, Index ii)

module Set = struct
      let ii new_val (Index fi, Index _) = (Index fi, Index new_val)
      let fi new_val (Index _, Index ii) = (Index new_val, Index ii)
end

module Get = struct
      let ii (Index _, Index res) = res
      let fi (Index res, Index _) = res
end

let shift_i get set shift (t: t) =
      let fi = get t in
      set (fi + shift) t

let shift_fi = shift_i Get.fi Set.fi
let shift_ii = shift_i Get.ii Set.ii 

let shift fi ii = shift_fi fi >> shift_ii ii

