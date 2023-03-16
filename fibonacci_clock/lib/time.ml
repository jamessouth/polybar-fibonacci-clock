let time = Unix.localtime (Unix.time ())

let getHour h =
  match h < 13 with
  | true -> ( match h with 0 -> 12 | _ -> h)
  | false -> h - 12

let hour = getHour time.tm_hour
let min = time.tm_min
let sec = time.tm_sec

(* let base =
  [
    "%{F#002b36}%{B#ffffff}";
    "▕";
    "▕";
    "%{B#ff0000} ▕%{B#ffffff}";
    "  ▕";
    "%{B#ff0000}    ▕%{B#ffffff}";
    "        ";
  ] *)
let base =
  [
    "%{F#00ff00}█%{O4}";
    "%{F#ff0000}█%{O4}";
    "%{F#0000ff}██%{O4}";
    "%{F#0000ff}███%{O4}";
    "%{F#ffffff}█████%{O4}";
    "%{F#ffffff}████████";
  ]
