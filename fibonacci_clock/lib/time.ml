let time = Unix.localtime (Unix.time ())

let getHour h =
  match h < 13 with
  | true -> ( match h with 0 -> 12 | _ -> h)
  | false -> h - 12

let hour = getHour time.tm_hour
let min = time.tm_min
let sec = time.tm_sec
