open State
open For_ui

let rec loop state msg =
      match msg with
      | NavigationMsg Quit ->
            let folder = match state with 
            | View (folder, _) -> folder
            | Detail DetailState.(ItemEdit (folder, _)) -> folder
            | Detail DetailState.(FilterEdit (folder, _)) -> folder
            in 
            Parser.to_file "saved.csv" folder
      | msg -> 
            let state = update state msg in
            let msg = Ui.draw state in
            loop state msg
 
let _ = loop (View Parser.(of_file "data.csv", Input.empty)) (NavigationMsg Nothing)

