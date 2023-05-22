open Core

(* (name * [accuracy ; sequence]) *)
(* let sequence_data =
   [
     ("semi-fibonacci", ([ 1; 1; 2; 1; 3; 2; 5 ], 15));
     ("tetranacci-numbers", ([ 1; 1; 1; 1; 4; 7 ], 15));
     ("tribonacci-numbers", ([ 1; 1; 2; 4; 7 ], 15));
     ("padovan-numbers-15", ([ 1; 1; 1; 1; 2; 2; 3; 4 ], 15));
     ("pascals-triangle-15", ([ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1 ], 15));
     ("divisors-of-928-15", ([ 1; 2; 4; 8 ], 15));
     ("fibonacci-triangle-15", ([ 1; 1; 1; 2; 1; 2; 3; 2; 2 ], 15));
     ("fibonacci-numbers", ([ 1; 1; 2; 3; 5; 8 ], 20));
     ("padovan-numbers-20", ([ 1; 1; 1; 1; 2; 2; 3; 4; 5 ], 20));
     ("pascals-triangle-20", ([ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4 ], 20));
     ("a034298", ([ 1; 2; 3; 4; 6; 6; 8 ], 30));
     ("partition-numbers", ([ 1; 1; 2; 3; 5; 7; 11 ], 30));
     ("pascals-triangle-30", ([ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4; 6; 4 ], 30));
     ("fibonacci-triangle-30", ([ 1; 1; 1; 2; 1; 2; 3; 2; 2; 3; 5; 3; 4 ], 30));
     ("divisors-of-928-30", ([ 1; 2; 4; 8; 16 ], 30));
     ("padovan-numbers-30", ([ 1; 2; 2; 3; 4; 5; 7; 9 ], 30));
     ("divisors-of-928-60", ([ 1; 2; 4; 8; 16; 29 ], 60));
     ("narayanas-cows", ([ 1; 1; 1; 2; 3; 4; 6; 9; 13; 19 ], 60));
     ("a331072", ([ 1; 2; 3; 5; 6; 8; 9; 12; 14 ], 60));
     ("padovan-numbers-60", ([ 1; 2; 2; 3; 4; 5; 7; 9; 12; 16 ], 60));
   ] *)

let sequences =
  Command.Arg_type.of_map ~accept_unique_prefixes:false ~case_sensitive:false
    ~list_values_in_help:true
    (String.Map.of_alist_exn
       [
         ("semi-fibonacci", ([ 1; 1; 2; 1; 3; 2; 5 ], 15));
         ("tetranacci-numbers", ([ 1; 1; 1; 1; 4; 7 ], 15));
         ("tribonacci-numbers", ([ 1; 1; 2; 4; 7 ], 15));
         ("padovan-numbers-15", ([ 1; 1; 1; 1; 2; 2; 3; 4 ], 15));
         ("pascals-triangle-15", ([ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1 ], 15));
         ("divisors-of-928-15", ([ 1; 2; 4; 8 ], 15));
         ("fibonacci-triangle-15", ([ 1; 1; 1; 2; 1; 2; 3; 2; 2 ], 15));
         ("fibonacci-numbers", ([ 1; 1; 2; 3; 5; 8 ], 20));
         ("padovan-numbers-20", ([ 1; 1; 1; 1; 2; 2; 3; 4; 5 ], 20));
         ("pascals-triangle-20", ([ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4 ], 20));
         ("a034298", ([ 1; 2; 3; 4; 6; 6; 8 ], 30));
         ("partition-numbers", ([ 1; 1; 2; 3; 5; 7; 11 ], 30));
         ( "pascals-triangle-30",
           ([ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4; 6; 4 ], 30) );
         ( "fibonacci-triangle-30",
           ([ 1; 1; 1; 2; 1; 2; 3; 2; 2; 3; 5; 3; 4 ], 30) );
         ("divisors-of-928-30", ([ 1; 2; 4; 8; 16 ], 30));
         ("padovan-numbers-30", ([ 1; 2; 2; 3; 4; 5; 7; 9 ], 30));
         ("divisors-of-928-60", ([ 1; 2; 4; 8; 16; 29 ], 60));
         ("narayanas-cows", ([ 1; 1; 1; 2; 3; 4; 6; 9; 13; 19 ], 60));
         ("a331072", ([ 1; 2; 3; 5; 6; 8; 9; 12; 14 ], 60));
         ("padovan-numbers-60", ([ 1; 2; 2; 3; 4; 5; 7; 9; 12; 16 ], 60));
       ])

let () =
  Command_unix.run ~version:"1.0" ~build_info:"RWO"
    (Command.basic ~summary:"fib clock"
       ~readme:(fun () -> "enter one or two of each flag")
       (let%map_open.Command args = args
        and seq =
          flag "-seq" (one_or_more_as_list sequences) ~doc:"string sequences"
        and gap =
          flag "-gap" (one_or_more_as_list int) ~doc:"int gap in pixels"
and spaces =
flag "-s"
  (optional_with_default 2 int)
  ~doc:"int Spaces between clocks when using two (default 2)"

  and colors = anon (sequence ("colors" %: string))

        in
        fun () ->
          if List.count args ~f:(fun x -> String.(=) x "-seq") <> List.count args ~f:(fun x -> String.(=) x "-gap") then failwith "number of sequences and gaps entered must be equal" else
          List.iter args ~f:(fun y -> Stdlib.print_string (y ^ " "));
          Stdlib.print_newline ();
          Stdlib.print_int spaces;
          Stdlib.print_newline ();
          List.iter seq ~f:(fun y ->
              Stdlib.print_int (snd y);
              List.iter (fst y) ~f:(fun z -> Stdlib.print_int z));
          Stdlib.print_newline ();
          List.iter gap ~f:(fun y -> Stdlib.print_int y);
          Stdlib.print_newline ();
          List.iter colors ~f:(fun y -> Stdlib.print_string y);
          Stdlib.print_newline ()))
