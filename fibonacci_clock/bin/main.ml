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

let accuracy_modes =
  Command.Arg_type.of_map ~accept_unique_prefixes:false ~case_sensitive:false
    ~list_values_in_help:true
    (String.Map.of_alist_exn
       [
         ("bars", Fibonacci_clock.Time.Bars);
         ("invert", Invert);
         ("lines", Lines);
         ("text", Text);
       ])

let color_profiles =
  Command.Arg_type.of_map ~accept_unique_prefixes:false ~case_sensitive:false
    ~list_values_in_help:true
    (String.Map.of_alist_exn
       [
         ("rgb", [ "#FFFFFF"; "#FF0A0A"; "#0AFF0A"; "#0A0AFF" ]);
         ("mondrian", [ "#FFFFFF"; "#FF0A0A"; "#F8DE00"; "#0A0AFF" ]);
         ("basbrun", [ "#FFFFFF"; "#502800"; "#14C814"; "#FF640A" ]);
         ("80's", [ "#FFFFFF"; "#F564C9"; "#72F736"; "#71EBDB" ]);
         ("pastel", [ "#FFFFFF"; "#FF7B7B"; "#8FFF70"; "#7878FF" ]);
         ("modern", [ "#FFFFFF"; "#D4312D"; "#91D231"; "#8D5FE0" ]);
         ("cold", [ "#FFFFFF"; "#D13EC8"; "#45E8E0"; "#5046CA" ]);
         ("warm", [ "#FFFFFF"; "#ED1414"; "#F6F336"; "#FF7E15" ]);
         ("earth", [ "#FFFFFF"; "#462300"; "#467A0A"; "#C8B600" ]);
         ("dark", [ "#FFFFFF"; "#D32222"; "#50974E"; "#101895" ]);
       ])

let () =
  Command_unix.run ~version:"1.0" ~build_info:"RWO"
    (Command.basic ~summary:"fib clock"
       ~readme:(fun () -> "enter one or two of each flag")
       (let%map_open.Command args = args
        and seqs =
          flag "-seq" (one_or_more_as_pair sequences) ~doc:"string sequences"
        and modes =
          flag "-mode"
            (one_or_more_as_pair accuracy_modes)
            ~doc:"string accuracy modes"
        and gaps =
          flag "-gap" (one_or_more_as_pair int) ~doc:"int gap in pixels"
        and spaces =
          flag "-spaces"
            (optional_with_default 2 int)
            ~doc:"int Spaces between clocks when using two (default 2)"
        and profiles =
          flag "-profiles" (optional color_profiles)
            ~doc:
              "string predefined color profiles for minute clocks; omit to \
               enter custom colors"
        and colors = anon (sequence ("colors" %: string)) in
        fun () ->
          let open Fibonacci_clock.Time in
          let countargs s = List.count args ~f:(fun x -> String.( = ) x s) in
          let numseqs = countargs "-seq" and numgaps = countargs "-gap" in
          if numseqs <> numgaps || numgaps <> countargs "-mode" then
            failwith
              "number of sequences, modes, and gaps entered must be equal"
          else
            let seq, acc = fst seqs
            and acc_mode = fst modes
            and gap = fst gaps in
            if gap < 0 then failwith "gaps must be unsigned integers"
            else
              let first = { seq; gap; acc; acc_mode; colors } in
              if numseqs = 1 then
                match (profiles, List.length colors) with
                | None, 2 -> Seconds first |> main
                | None, 4 -> Minutes first |> main
                | Some profile, _ ->
                    Minutes { first with colors = profile } |> main
                | None, _ ->
                    failwith
                      "invalid input - enter 2 or 4 colors or choose a \
                       profile. Escape # or quote each color"
              else if numseqs = 2 then
                let seq1, acc1 = List.hd_exn (snd seqs)
                and acc_mode1 = List.hd_exn (snd modes)
                and gap1 = List.hd_exn (snd gaps) in
                if gap1 < 0 then failwith "gaps must be unsigned integers"
                else
                  match (profiles, List.length colors) with
                  | None, 6 ->
                      let min_colors, sec_colors = List.split_n colors 4 in
                      Both
                        ( { first with colors = min_colors },
                          spaces,
                          {
                            seq = seq1;
                            gap = gap1;
                            acc = acc1;
                            acc_mode = acc_mode1;
                            colors = sec_colors;
                          } )
                      |> main
                  | None, _ ->
                      failwith
                        "invalid input - enter 6 colors or choose a profile \
                         for the minutes clock and enter 2 colors for the \
                         seconds clock. Escape # or quote each color"
                  | Some profile, 2 ->
                      Both
                        ( { first with colors = profile },
                          spaces,
                          {
                            seq = seq1;
                            gap = gap1;
                            acc = acc1;
                            acc_mode = acc_mode1;
                            colors;
                          } )
                      |> main
                  | Some _, _ ->
                      failwith
                        "invalid input - enter 2 colors for the seconds clock. \
                         Escape # or quote each color"
              else failwith "invalid input - enter one or two of each flag"))
