open Utils 

type 'a index_type = Index of int

type t = Filter.t index_type * Item.t index_type

let empty = Index 0, Index 0

module Set = struct
      let ii new_val ((Index fi, Index _), _) = (Index fi, Index new_val)
      let fi new_val ((Index _, Index ii), _) = (Index new_val, Index ii)
end

module Get = struct
      let ii ((Index _, Index res), _) = res
      let fi ((Index res, Index _), _) = res
end

let normalize_index xs =
      max 0
      >> min List.((length xs) - 1)

let shift_filter filters shift (Index fi, Index ii) =
      let fi = normalize_index filters (fi + shift) in
      (Index fi, Index ii)

let shift_item items shift (Index fi, Index ii) =
      let ii = normalize_index items (ii + shift) in
      (Index fi, Index ii)

let normalize filters items =
      shift_filter filters 0
      >> shift_item items 0

