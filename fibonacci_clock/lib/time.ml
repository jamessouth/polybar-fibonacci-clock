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

(* let rec take n l =
   if n = 0 then []
   else match l with [] -> [] | hd :: tl -> hd :: take (n - 1) tl *)

(* let rec flatten_pairs = function
   | [] -> []
   | (a, b) :: tl -> (a ^ b) :: flatten_pairs tl *)

(* let mono =
     [
       (* 15 *)
       ("Semi_Fibonacci", [ 1; 1; 2; 1; 3; 2; 5 ]);
       ("Tetranacci_numbers", [ 1; 1; 1; 1; 4; 7 ]);
       ("Tribonacci_numbers", [ 1; 1; 2; 4; 7 ]);
       (* 20 *)
       ("Fibonacci_numbers", [ 1; 1; 2; 3; 5; 8 ]);
       (* 30 *)
       ("A034298", [ 1; 2; 3; 4; 6; 6; 8 ]);
       ("Partition_numbers", [ 1; 1; 2; 3; 5; 7; 11 ]);
       (* 60 *)
       ("Narayanas_cows", [ 1; 1; 1; 2; 3; 4; 6; 9; 13; 19 ]);
       ("A331072", [ 1; 2; 3; 5; 6; 8; 9; 12; 14 ]);
     ]

   let poly =
     [
       ("Padovan_numbers_sm", [ [ 1; 1; 1; 1; 2; 2; 3; 4 ]; [ 5 ] ]);
       ( "Pascals_triangle",
         [ [ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1 ]; [ 1; 4 ]; [ 6; 4 ] ] );
       ("Fibonacci_triangle", [ [ 1; 1; 1; 2; 1; 2; 3; 2; 2 ]; []; [ 3; 5; 3; 4 ] ]);
       ("Divisors_of_928", [ [ 1; 2; 4; 8 ]; []; [ 16 ]; [ 29 ] ]);
       ("Padovan_numbers_lg", [ []; []; [ 1; 2; 2; 3; 4; 5; 7; 9 ]; [ 12; 16 ] ]);
     ] *)

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
