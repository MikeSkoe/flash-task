let rec loop state msg =
      match msg with
      | State.(NavigationMsg Quit) -> ()
      | msg -> 
            let state = State.update state msg in
            let msg = Ui.draw state in
            loop state msg
 
(* TODO: save data (done/archived/deleted/location) to file on exit *)

let _ = loop State.(of_file "data.csv") State.(NavigationMsg Nothing)

