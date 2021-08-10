let rec loop state msg =
      match msg with
      | State.(NavigationMsg Quit) -> State.to_file "saved.csv" state
      | msg -> 
            let state = State.update state msg in
            let msg = Ui.draw state in
            loop state msg

(*
let _ = State.(loop debug (NavigationMsg Nothing))
*)
 
let _ = loop State.(of_file "data.csv") State.(NavigationMsg Nothing)

