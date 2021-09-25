type 'a t = 'a list

let push a t = a :: t
let back = function
      | [] -> []
      | _ :: xs -> xs
let shift a = function
      | [] -> a :: []
      | _ :: xs -> a :: xs
let make a = a :: []

