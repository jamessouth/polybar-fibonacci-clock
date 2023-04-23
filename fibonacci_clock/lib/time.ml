let get_hour hr =
  match (hr < 13, hr = 0) with
  | true, true -> 12
  | true, false -> hr
  | _ -> hr - 12

let { Unix.tm_sec = sec; tm_min = min; tm_hour = hour; _ } =
  Unix.localtime (Unix.time ())

(* let u2591 = "░"
   let u2592 = "▒"
   let u2593 = "▓"
   let u2588 = "█" *)

(* let blocks = [ "░"; "▒"; "▓"; "█" ] *)

(* +(4-(60/x)) *)

(* let rec take n l =
   if n = 0 then []
   else match l with [] -> [] | hd :: tl -> hd :: take (n - 1) tl *)

(* let rec flatten_pairs = function
   | [] -> []
   | (a, b) :: tl -> (a ^ b) :: flatten_pairs tl *)

(* let rec repeat_str str n =
   if n > -1 then match n with 0 -> "" | _ -> str ^ repeat_str str (n - 1)
   else "" *)

let base =
  [
    "%{F#00ff00}█";
    "%{F#ff0000}█";
    "%{F#0000ff}██";
    "%{F#0000ff}███";
    "%{F#ffffff}█████";
    "%{F#ffffff}████████";
  ]

let pbase str gap =
  let g = "%{O" ^ string_of_int gap ^ "}" in
  print_endline (String.concat g str)

let pp = pbase base
