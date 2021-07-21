let rec loop state msg =
      match msg with
      | State.Quit -> ()
      | msg -> 
            let state = State.update state msg in
            let msg = Ui.draw state in
            loop state msg

let _ = loop State.empty State.Nothing

