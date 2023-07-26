open Core



type sequence = Sequence of int list * Fibonacci_clock.Time.accuracy_level

let sequence_to_tuple = function Sequence (ints, acc) -> (ints, acc)


let sequences =
  Command.Arg_type.of_map ~accept_unique_prefixes:false ~case_sensitive:false
    ~list_values_in_help:true
    (String.Map.of_alist_exn
       [
         ( "semi-fibonacci-15", Sequence ([ 1; 1; 2; 1; 3; 2; 5 ], Fifteen) );
         ("tetranacci-numbers-15", Sequence ([ 1; 1; 1; 1; 4; 7 ], Fifteen));
         ("tribonacci-numbers-15", Sequence ([ 1; 1; 2; 4; 7 ], Fifteen));
         ("padovan-numbers-15", Sequence ([ 1; 1; 1; 1; 2; 2; 3; 4 ], Fifteen));
         ("pascals-triangle-15", Sequence ([ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1 ], Fifteen));
         ("divisors-of-928-15", Sequence ([ 1; 2; 4; 8 ], Fifteen));
         ("fibonacci-triangle-15", Sequence ([ 1; 1; 1; 2; 1; 2; 3; 2; 2 ], Fifteen));

         ("fibonacci-numbers-20", Sequence ([ 1; 1; 2; 3; 5; 8 ], Twenty));
         ("padovan-numbers-20", Sequence ([ 1; 1; 1; 1; 2; 2; 3; 4; 5 ], Twenty));
         ( "pascals-triangle-20",
           Sequence ([ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4 ], Twenty) );

         ("a034298-30", Sequence ([ 1; 2; 3; 4; 6; 6; 8 ], Thirty));
         ("partition-numbers-30", Sequence ([ 1; 1; 2; 3; 5; 7; 11 ], Thirty));
         ( "pascals-triangle-30",
           Sequence ([ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4; 6; 4 ], Thirty) );
         ( "fibonacci-triangle-30",
           Sequence ([ 1; 1; 1; 2; 1; 2; 3; 2; 2; 3; 5; 3; 4 ], Thirty) );
         ("divisors-of-928-30", Sequence ([ 1; 2; 4; 8; 16 ], Thirty));
         ("padovan-numbers-30", Sequence ([ 1; 2; 2; 3; 4; 5; 7; 9 ], Thirty));
         
         ("divisors-of-928-60", Sequence ([ 1; 2; 4; 8; 16; 29 ], Sixty));
         ("narayanas-cows-60", Sequence ([ 1; 1; 1; 2; 3; 4; 6; 9; 13; 19 ], Sixty));
         ("a331072-60", Sequence ([ 1; 2; 3; 5; 6; 8; 9; 12; 14 ], Sixty));
         ("padovan-numbers-60", Sequence ([ 1; 2; 2; 3; 4; 5; 7; 9; 12; 16 ], Sixty));
       ])

let accuracy_modes =
  Command.Arg_type.of_map ~accept_unique_prefixes:false ~case_sensitive:false
    ~list_values_in_help:true
    (String.Map.of_alist_exn
       [
         ("bars", Fibonacci_clock.Time.By_char [ "▂"; "▄"; "▆"; "█" ]);
         ("invert colors", By_pb_format ["";"%{R}";"%{R}"]);
         ("overlines", By_pb_format ["";"%{+o}";"%{-o}"]);
         ("underlines", By_pb_format ["";"%{+u}";"%{-u}"]);
         ("both lines", By_pb_format ["";"%{+o}%{+u}";"%{-o}%{-u}"]);
         ("text", Text);
       ])

let color_profiles =
  Command.Arg_type.of_map ~accept_unique_prefixes:false ~case_sensitive:false
    ~list_values_in_help:true
    (String.Map.of_alist_exn
       [
         ("rgb", Fibonacci_clock.Time.Profile [ "#FFFFFF"; "#FF0A0A"; "#0AFF0A"; "#0A0AFF" ]);
         ("mondrian", Profile [ "#FFFFFF"; "#FF0A0A"; "#F8DE00"; "#0A0AFF" ]);
         ("basbrun", Profile [ "#FFFFFF"; "#502800"; "#14C814"; "#FF640A" ]);
         ("80's", Profile [ "#FFFFFF"; "#F564C9"; "#72F736"; "#71EBDB" ]);
         ("pastel", Profile [ "#FFFFFF"; "#FF7B7B"; "#8FFF70"; "#7878FF" ]);
         ("modern", Profile [ "#FFFFFF"; "#D4312D"; "#91D231"; "#8D5FE0" ]);
         ("cold", Profile [ "#FFFFFF"; "#D13EC8"; "#45E8E0"; "#5046CA" ]);
         ("warm", Profile [ "#FFFFFF"; "#ED1414"; "#F6F336"; "#FF7E15" ]);
         ("earth", Profile [ "#FFFFFF"; "#462300"; "#467A0A"; "#C8B600" ]);
         ("dark", Profile [ "#FFFFFF"; "#D32222"; "#50974E"; "#101895" ]);
       ])

let () =
  Command_unix.run ~version:"1.0" ~build_info:"RWO"
    (Command.basic ~summary:"fib clock"
       ~readme:(fun () ->
         "Enter one or two of each flag: one for minutes or seconds; two for \
          both: minutes first, then seconds. The numbers at the end of each \
          sequence name are the sum of the numbers in the sequence/the \
          accuracy of that sequence. A lower number/accuracy will generally \
          take up less space in your bar. The accuracy mode is the way these \
          sequences can still display time down to the minute or second. A sequence with 60 accuracy does not need an accuracy mode.")
       (let%map_open.Command args = args
        and gaps =
          flag "-gap" (one_or_more_as_pair int)
            ~doc:"int Pixel gap between numbers in the given clock"
        and modes =
          flag "-mode"
            (one_or_more_as_pair accuracy_modes)
            ~doc:"string Accuracy modes"
        and seqs =
          flag "-seq" (one_or_more_as_pair sequences) ~doc:"string Sequences"
        and profiles =
          flag "-profiles" (optional color_profiles)
            ~doc:
              "string Color palettes from the original Fibonacci clock. For \
               minute clocks only; omit to enter custom colors"
        and spaces =
          flag "-spaces"
            (optional_with_default 2 int)
            ~doc:"int Space gap between clocks when using both (default: 2)"
        and colors = anon (sequence ("colors" %: string)) in
        fun () ->
          let open Fibonacci_clock.Time in
          let gaps_error = "gaps must be integers >= 0" in
          let countargs s = List.count args ~f:(fun x -> String.( = ) x s) in
          let numseqs = countargs "-seq" and numgaps = countargs "-gap" in
          if numseqs <> numgaps || numgaps <> countargs "-mode" then
            failwith
              "number of sequences, modes, and gaps entered must be equal"
          else
            let seq, acc_lvl = sequence_to_tuple (fst seqs)
            and acc_mode = fst modes
            and gap = fst gaps in
            if gap < 0 then failwith gaps_error
            else
              let first = { seq; gap; acc_lvl; acc_mode; colors } in
              if numseqs = 1 then
                match (profiles, List.length colors) with
                | None, 2 -> Seconds first |> main
                | None, 4 -> Minutes first |> main
                | Some profile, _ ->
                    Minutes { first with colors = profile_to_list profile } |> main
                | None, _ ->
                    failwith
                      "invalid input - enter 2 or 4 colors or choose a \
                       profile. Escape # or quote each color"
              else if numseqs = 2 then
                let seq1, acc1 = snd seqs |> List.hd_exn |> sequence_to_tuple
                and acc_mode1 = List.hd_exn (snd modes)
                and gap1 = List.hd_exn (snd gaps) in
                if gap1 < 0 then failwith gaps_error
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
                            acc_lvl = acc1;
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
                        ( { first with colors = profile_to_list profile },
                          spaces,
                          {
                            seq = seq1;
                            gap = gap1;
                            acc_lvl = acc1;
                            acc_mode = acc_mode1;
                            colors;
                          } )
                      |> main
                  | Some _, _ ->
                      failwith
                        "invalid input - enter 2 colors for the seconds clock. \
                         Escape # or quote each color"
              else failwith "invalid input - enter one or two of each flag"))
