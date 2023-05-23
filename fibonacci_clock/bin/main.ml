open Core

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
        and seqs =
          flag "-seq" (one_or_more_as_pair sequences) ~doc:"string sequences"
        and gaps =
          flag "-gap" (one_or_more_as_pair int) ~doc:"int gap in pixels"
        and spaces =
          flag "-spaces"
            (optional_with_default 2 int)
            ~doc:"int Spaces between clocks when using two (default 2)"
        and colors = anon (sequence ("colors" %: string)) in

        fun () ->
          let open Fibonacci_clock.Time in
          if
            List.count args ~f:(fun x -> String.( = ) x "-seq")
            <> List.count args ~f:(fun x -> String.( = ) x "-gap")
          then failwith "number of sequences and gaps entered must be equal"
          else
            let seq, acc = fst seqs in
            let gap = fst gaps in
            let first = { seq; gap; acc; colors } in

            match
              ( List.count args ~f:(fun x -> String.( = ) x "-seq"),
                List.length colors )
            with
            | 1, 2 -> Seconds first |> main
            | 1, 4 -> Minutes first |> main
            | 2, 6 ->
                let seq1, acc1 = List.hd_exn (snd seqs) in
                let min_colors, sec_colors = List.split_n colors 4 in

                Both
                  ( { first with colors = min_colors },
                    spaces,
                    {
                      seq = seq1;
                      gap = List.hd_exn (snd gaps);
                      acc = acc1;
                      colors = sec_colors;
                    } )
                |> main
            | 1, _ ->
                failwith
                  "invalid input - enter 2 or 4 colors. Escape # or quote each \
                   color."
            | 2, _ ->
                failwith
                  "invalid input - enter 6 colors. Escape # or quote each \
                   color."
            | _ -> failwith "invalid input"))
