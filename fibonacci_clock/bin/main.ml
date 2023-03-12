let time = Unix.localtime (Unix.time ())
let rawhour = time.tm_hour
let min = time.tm_min
let sec = time.tm_sec

let hour rh =
  match rh < 13 with
  | true -> ( match rh with 0 -> 12 | _ -> rh)
  | false -> rh - 12
