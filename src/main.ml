open State
open Entities
open For_ui

let rec loop state msg =
      match msg with
      | NavigationMsg Quit ->
            let items = match state with 
            | View {items; _} -> items
            | Detail {items; _} -> items
            in 
            Parser.to_file "saved.csv" items
      | msg -> 
            let state = update state msg in
            let msg = Ui.draw state in
            loop state msg
 
let _ =
      let items, filters = Parser.of_file "data.csv" in
      let selected = Selected.empty in
      let input = Input.empty in
      let initial_state = View {items; filters; selected; input} in

      loop initial_state (NavigationMsg Nothing)

