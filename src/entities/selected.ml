open Utils 

type 'a index_type = Index of int

type t = Filter.t index_type * Item.t index_type

let empty = Index 0, Index 0

module Set = struct
      let ii new_val (Index fi, Index _) = (Index fi, Index new_val)
      let fi new_val (Index _, Index ii) = (Index new_val, Index ii)
end

module Get = struct
      let ii (Index _, Index res) = res
      let fi (Index res, Index _) = res
end

let normalize_index length =
      max 0
      >> min (length - 1)

let shift_filter filters_length shift (Index fi, Index ii) =
      let fi = normalize_index filters_length (fi + shift) in
      (Index fi, Index ii)

let shift_item items_length shift (Index fi, Index ii) =
      let ii = normalize_index items_length (ii + shift) in
      (Index fi, Index ii)

