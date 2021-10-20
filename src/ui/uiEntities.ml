open Utils
open Entities

module Tag = struct
      let draw =
            Tag.as_string
            >> Printf.sprintf "[%s]"
            >> Node.(text Secondary)
end

module Item = struct
      let draw ~item ~is_selected =
            let style =
                  if is_selected
                  then Node.Selected
                  else Node.Normal
            in
            let title =
                Item.Get.title item
                |> Node.(text style)
            in
            title
end

module Filter = struct
      let draw ~filter ~is_selected =
          let style =
              if is_selected
              then Node.Underline
              else Node.Normal
          in
          let title =
              Filter.Get.title filter
              |> Node.(text style)
          in
          title
end

