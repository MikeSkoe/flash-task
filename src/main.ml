open State

let rec loop state msg =
      match msg with
      | NavigationMsg Quit ->
            let folder = match state with 
            | View (folder, _) -> folder
            | Detail (folder, _, _) -> folder
            in 
            Parser.to_file "saved.csv" folder
      | msg -> 
            let state = update state msg in
            let msg = Ui.draw state in
            loop state msg
 
let _ = loop (View Parser.(of_file "data.csv", Selected.empty)) (NavigationMsg Nothing)

