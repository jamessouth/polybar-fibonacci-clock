let time = Unix.localtime (Unix.time ())

let getHour h =
  match h < 13 with
  | true -> ( match h with 0 -> 12 | _ -> h)
  | false -> h - 12

let hour = getHour time.tm_hour
let min = time.tm_min
let sec = time.tm_sec

let u2591 = "░"
let u2592 = "▒"
let u2593 = "▓"
let u2588 = "█"

let base =
  [
    "%{F#00ff00}█";
    "%{F#ff0000}█";
    "%{F#0000ff}██";
    "%{F#0000ff}███";
    "%{F#ffffff}█████";
    "%{F#ffffff}████████";
  ]

  
let pbase str gap = let g = "%{O" ^ (string_of_int gap) ^ "}" in print_endline (String.concat g str)

let pp = pbase base 