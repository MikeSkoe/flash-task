module State' = State.Make (Db.Api)

let rec loop state msg =
      match msg with
      | State.NavigationMsg Quit -> ()
      | msg -> 
            let state = State'.update state msg in
            let msg = Ui.draw state in
            loop state msg

let _ = loop (State.View State.ViewState.empty) (State.ViewMsg State.ViewState.Init)
