let time = Unix.localtime (Unix.time ())

let getHour h =
  match h < 13 with
  | true -> ( match h with 0 -> 12 | _ -> h)
  | false -> h - 12

let hour = getHour time.tm_hour
let min = time.tm_min
let sec = time.tm_sec
(* let u2591 = "░"
   let u2592 = "▒"
   let u2593 = "▓"
   let u2588 = "█" *)

let blocks = [ "█"; "▓"; "▒"; "░" ]

let rec take n l =
  if n = 0 then [] else match l with [] -> [] | h :: t -> h :: take (n - 1) t

let rec flatten_pairs = function
  | [] -> []
  | h :: t -> (fst h ^ snd h) :: flatten_pairs t

let sequences =
  [
    ("Fibonacci_numbers", [ []; [ 1; 1; 2; 3; 5; 8 ] ]);
    ("A331072", [ []; []; []; [ 1; 2; 3; 5; 6; 8; 9; 12; 14 ] ]);
    ("Fibonacci_triangle", [ [ 1; 1; 1; 2; 1; 2; 3; 2; 2 ]; []; [ 3; 5; 3; 4 ] ]);
    ("A034298", [ []; []; [ 1; 2; 3; 4; 6; 6; 8 ] ]);
    ("Semi_Fibonacci", [ [ 1; 1; 2; 1; 3; 2; 5 ] ]);
    ("Divisors_of_928", [ [ 1; 2; 4; 8 ]; []; [ 16 ]; [ 29 ] ]);
    ( "Pascals_triangle",
      [ [ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1 ]; [ 1; 4 ]; [ 6; 4 ] ] );
    ("Padovan_numbers_sm", [ [ 1; 1; 1; 1; 2; 2; 3; 4 ]; [ 5 ] ]);
    ("Padovan_numbers_lg", [ []; []; [ 1; 2; 2; 3; 4; 5; 7; 9 ]; [ 12; 16 ] ]);
    ("Narayanas_cows", [ []; []; []; [ 1; 1; 1; 2; 3; 4; 6; 9; 13; 19 ] ]);
    ("Tetranacci_numbers", [ [ 1; 1; 1; 1; 4; 7 ] ]);
    ("Tribonacci_numbers", [ [ 1; 1; 2; 4; 7 ] ]);
    ("Partition_numbers", [ []; [ 1; 1; 2; 3; 5; 7 ]; [ 11 ] ]);
  ]

let rec repeat s n =
  if n > -1 then match n with 0 -> "" | _ -> s ^ repeat s (n - 1) else ""

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
