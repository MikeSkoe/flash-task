open Utils
open Entities

module Tag = struct
      let draw =
            Tag.as_string
            >> Printf.sprintf "[%s]"
            >> Node.(text Secondary)
end

module Item = struct
      let draw (item: Item.t) is_selected =
            let style =
                  if is_selected
                  then Node.Selected
                  else Node.Normal
            in
            let title = Node.(text style item.title) in
            title
end

module Filter = struct
      let draw filter is_selected =
            let title =
                  Filter.Get.title filter
                  |> Node.(text (if is_selected then Underline else Normal))
            in
            title
end

