open Core

type seq = { name : string; _seq : int list; _acc : int }

let sequence_data =
  [
    (* { name = "Semi_Fibonacci"; _seq = [ 1; 1; 2; 1; 3; 2; 5 ]; _acc = 15 }; *)
    (* { name = "Tetranacci_numbers"; _seq = [ 1; 1; 1; 1; 4; 7 ]; _acc = 15 }; *)
    (* { name = "Tribonacci_numbers"; _seq = [ 1; 1; 2; 4; 7 ]; _acc = 15 }; *)
    (* { name = "Padovan_numbers_15"; _seq = [ 1; 1; 1; 1; 2; 2; 3; 4 ]; _acc = 15 }; *)
    (* {
      name = "Pascals_triangle_15";
      _seq = [ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1 ];
      _acc = 15;
    }; *)
    (* { name = "Divisors_of_928_15"; _seq = [ 1; 2; 4; 8 ]; _acc = 15 }; *)
    (* {
      name = "Fibonacci_triangle_15";
      _seq = [ 1; 1; 1; 2; 1; 2; 3; 2; 2 ];
      _acc = 15;
    }; *)
    (* { name = "Fibonacci_numbers"; _seq = [ 1; 1; 2; 3; 5; 8 ]; _acc = 20 }; *)
    (* {
      name = "Padovan_numbers_20";
      _seq = [ 1; 1; 1; 1; 2; 2; 3; 4; 5 ];
      _acc = 20;
    }; *)
    (* {
      name = "Pascals_triangle_20";
      _seq = [ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4 ];
      _acc = 20;
    }; *)
    (* { name = "A034298"; _seq = [ 1; 2; 3; 4; 6; 6; 8 ]; _acc = 30 }; *)
    (* { name = "Partition_numbers"; _seq = [ 1; 1; 2; 3; 5; 7; 11 ]; _acc = 30 }; *)
    (* {
      name = "Pascals_triangle_30";
      _seq = [ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4; 6; 4 ];
      _acc = 30;
    }; *)
    {
      name = "Fibonacci_triangle_30";
      _seq = [ 1; 1; 1; 2; 1; 2; 3; 2; 2; 3; 5; 3; 4 ];
      _acc = 30;
    };
    { name = "Divisors_of_928_30"; _seq = [ 1; 2; 4; 8; 16 ]; _acc = 30 };
    { name = "Padovan_numbers_30"; _seq = [ 1; 2; 2; 3; 4; 5; 7; 9 ]; _acc = 30 };
    { name = "Divisors_of_928_60"; _seq = [ 1; 2; 4; 8; 16; 29 ]; _acc = 60 };
    {
      name = "Narayanas_cows";
      _seq = [ 1; 1; 1; 2; 3; 4; 6; 9; 13; 19 ];
      _acc = 60;
    };
    { name = "A331072"; _seq = [ 1; 2; 3; 5; 6; 8; 9; 12; 14 ]; _acc = 60 };
    {
      name = "Padovan_numbers_60";
      _seq = [ 1; 2; 2; 3; 4; 5; 7; 9; 12; 16 ];
      _acc = 60;
    };
  ]

(* let check_color_list = function
    | [ _; _ ]
    | [ _; _;_;_ ] as a -> List.map a ~f:(fun x -> if String.is_prefix x ~prefix:"#" then x else "#" ^ x)
    | [ _ ] | _ -> failwith "invalid input" *)

    let semi_Fibonacci = let gg = [ 1; 1; 2; 1; 3; 2; 5 ]
in      Command.basic
        ~summary:"[ 1; 1; 2; 1; 3; 2; 5 ]"
        (let%map_open.Command path = path and
        gap = anon ("gap" %: int) and 
         colors = anon (sequence ("colors" %: string)) in
          fun () -> Stdlib.print_int (Stdlib.List.length path+Stdlib.List.length gg+ gap+Stdlib.List.length colors))

    let tetranacci_numbers = let gg = [ 1; 1; 1; 1; 4; 7 ]
in      Command.basic
        ~summary:"[ 1; 1; 1; 1; 4; 7 ]"
        (let%map_open.Command path = path and
        gap = anon ("gap" %: int) and 
         colors = anon (sequence ("colors" %: string)) in
          fun () -> Stdlib.print_int (Stdlib.List.length path+Stdlib.List.length gg+ gap+Stdlib.List.length colors))

    let tribonacci_numbers = let gg = [ 1; 1; 2; 4; 7 ]
in      Command.basic
        ~summary:"[ 1; 1; 2; 4; 7 ]"
        (let%map_open.Command path = path and
        gap = anon ("gap" %: int) and 
         colors = anon (sequence ("colors" %: string)) in
          fun () -> Stdlib.print_int (Stdlib.List.length path+Stdlib.List.length gg+ gap+Stdlib.List.length colors))
    let padovan_numbers_15 = let gg = [ 1; 1; 1; 1; 2; 2; 3; 4 ]
in      Command.basic
        ~summary:"[ 1; 1; 1; 1; 2; 2; 3; 4 ]"
        (let%map_open.Command path = path and
        gap = anon ("gap" %: int) and 
         colors = anon (sequence ("colors" %: string)) in
          fun () -> Stdlib.print_int (Stdlib.List.length path+Stdlib.List.length gg+ gap+Stdlib.List.length colors))
    let pascals_triangle_15 = let gg = [ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1 ]
in      Command.basic
        ~summary:"[ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1 ]"
        (let%map_open.Command path = path and
        gap = anon ("gap" %: int) and 
         colors = anon (sequence ("colors" %: string)) in
          fun () -> Stdlib.print_int (Stdlib.List.length path+Stdlib.List.length gg+ gap+Stdlib.List.length colors))
    let divisors_of_928_15 = let gg = [ 1; 2; 4; 8 ]
in      Command.basic
        ~summary:"[ 1; 2; 4; 8 ]"
        (let%map_open.Command path = path and
        gap = anon ("gap" %: int) and 
         colors = anon (sequence ("colors" %: string)) in
          fun () -> Stdlib.print_int (Stdlib.List.length path+Stdlib.List.length gg+ gap+Stdlib.List.length colors))
    let fibonacci_triangle_15 = let gg = [ 1; 1; 1; 2; 1; 2; 3; 2; 2 ]
in      Command.basic
        ~summary:"[ 1; 1; 1; 2; 1; 2; 3; 2; 2 ]"
        (let%map_open.Command path = path and
        gap = anon ("gap" %: int) and 
         colors = anon (sequence ("colors" %: string)) in
          fun () -> Stdlib.print_int (Stdlib.List.length path+Stdlib.List.length gg+ gap+Stdlib.List.length colors))
    let fibonacci_numbers = let gg = [ 1; 1; 2; 3; 5; 8 ]
in      Command.basic
        ~summary:"[ 1; 1; 2; 3; 5; 8 ]"
        (let%map_open.Command path = path and
        gap = anon ("gap" %: int) and 
         colors = anon (sequence ("colors" %: string)) in
          fun () -> Stdlib.print_int (Stdlib.List.length path+Stdlib.List.length gg+ gap+Stdlib.List.length colors))
    let padovan_numbers_20 = let gg = [ 1; 1; 1; 1; 2; 2; 3; 4; 5 ]
in      Command.basic
        ~summary:"[ 1; 1; 1; 1; 2; 2; 3; 4; 5 ]"
        (let%map_open.Command path = path and
        gap = anon ("gap" %: int) and 
         colors = anon (sequence ("colors" %: string)) in
          fun () -> Stdlib.print_int (Stdlib.List.length path+Stdlib.List.length gg+ gap+Stdlib.List.length colors))
    let pascals_triangle_20 = let gg = [ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4 ]
in      Command.basic
        ~summary:"[ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4 ]"
        (let%map_open.Command path = path and
        gap = anon ("gap" %: int) and 
         colors = anon (sequence ("colors" %: string)) in
          fun () -> Stdlib.print_int (Stdlib.List.length path+Stdlib.List.length gg+ gap+Stdlib.List.length colors))
    let a034298 = let gg = [ 1; 2; 3; 4; 6; 6; 8 ]
in      Command.basic
        ~summary:"[ 1; 2; 3; 4; 6; 6; 8 ]"
        (let%map_open.Command path = path and
        gap = anon ("gap" %: int) and 
         colors = anon (sequence ("colors" %: string)) in
          fun () -> Stdlib.print_int (Stdlib.List.length path+Stdlib.List.length gg+ gap+Stdlib.List.length colors))
    let partition_numbers = let gg = [ 1; 1; 2; 3; 5; 7; 11 ]
in      Command.basic
        ~summary:"[ 1; 1; 2; 3; 5; 7; 11 ]"
        (let%map_open.Command path = path and
        gap = anon ("gap" %: int) and 
         colors = anon (sequence ("colors" %: string)) in
          fun () -> Stdlib.print_int (Stdlib.List.length path+Stdlib.List.length gg+ gap+Stdlib.List.length colors))
    let pascals_triangle_30 = let gg = [ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4; 6; 4 ]
in      Command.basic
        ~summary:"[ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4; 6; 4 ]"
        (let%map_open.Command path = path and
        gap = anon ("gap" %: int) and 
         colors = anon (sequence ("colors" %: string)) in
          fun () -> Stdlib.print_int (Stdlib.List.length path+Stdlib.List.length gg+ gap+Stdlib.List.length colors))

let minutes =
  Command.basic ~summary:"An hours/minutes clock"
  ~readme:(fun () ->
    "Takes a sequence name from the(string), a gap (int), and four 6-digit hex colors with leading # (off, minutes, hours, both)\nEx: A331072 5 #ffffff #00ff00 #ff0000 #0000ff")
    (let%map_open.Command seq = anon ("sequence" %: string)
     and gap = anon ("gap" %: int)
     and colors = anon (sequence ("colors" %: string)) in
     fun () ->
       let () =
         if
           not (List.exists sequence_data ~f:(fun x -> String.( = ) x.name seq))
         then failwith (seq ^ " is not a valid sequence option")
       in
       let () =
         if gap < 0 then failwith "gap must be a positive integer or zero"
       in
       let () =
         if Int.( <> ) (List.length colors) 4 then
           failwith
             "enter four 6-digit hex colors, with or without a leading '#'"
       in
       print_string seq;
       Stdlib.print_int gap;
       List.iter colors ~f:Stdlib.print_string)

(* let seconds =
  Command.basic ~summary:"A seconds clock"
    ~readme:(fun () ->
      "Takes a sequence, a gap, and two hex colors (off and on)")
    (let%map_open.Command seq = anon ("sequence" %: string)
     and gap = anon ("gap" %: int)
     and colors = anon (sequence ("colors" %: string)) in
     fun () ->
       let () =
         if
           not (List.exists sequence_data ~f:(fun x -> String.( = ) x.name seq))
         then failwith (seq ^ " is not a valid sequence option")
       in
       let () =
         if gap < 0 then failwith "gap must be a positive integer or zero"
       in
       let () =
         if Int.( <> ) (List.length colors) 2 then
           failwith
             "enter four 6-digit hex colors, with or without a leading '#'"
       in
       print_string seq;
       Stdlib.print_int gap;
       List.iter colors ~f:Stdlib.print_string) *)

(* let both =
  Command.basic ~summary:"An hours/minutes clock and a seconds clock"
    ~readme:(fun () ->
      "Takes two sequences, two gaps, the number of spaces to put between the \
       clocks, and six hex colors")
    (let%map_open.Command seqs = anon (t2 ("seq0" %: string) ("seq1" %: string))
     and gaps = anon (t2 ("gap0" %: int) ("gap1" %: int))
     and spaces = anon ("spaces" %: int)
     and colors = anon (sequence ("colors" %: string)) and help = help
     and path = path
     and args = args in
     fun () ->
      print_string (Command.summary seconds);
       print_string (fst seqs ^ snd seqs);
       Stdlib.print_int (fst gaps + snd gaps + spaces);
       Stdlib.print_int (Stdlib.List.length colors);
       print_endline "PATH:";
         List.iter path ~f:(fun x -> print_endline ("  " ^ x));
         print_endline "ARGS:";
         List.iter args ~f:(fun x -> print_endline ("  " ^ x));
         print_endline "HELP!";
         print_endline (Lazy.force help)) *)

let seconds =
  Command.group ~summary:"\nA Fibonacci clock for polybar"
    ~readme:(fun () -> "More detailed information")
    ~preserve_subcommand_order:()
    [ ("semi-Fib", semi_Fibonacci) ]


let command_0 =
  Command.group ~summary:"\nA Fibonacci clock for polybar"
    ~readme:(fun () -> "More detailed information")
    ~preserve_subcommand_order:()
    [ ("seconds", seconds); ("minutes", minutes);
     (* ("both", both)  *)
     ]

(* let command =
   Command.basic ~summary:"fib clock"
     ~readme:(fun () -> "More detailed information")
     (let%map_open.Command first_seq =
        flag "-s1" (required layout) ~doc:"layout for first clock"
      and first_cols =
        flag "-c1" (required color_list) ~doc:"colors for first clock"
      and second =
        flag "-s2" (optional layout) ~doc:"layout for second clock"
      and space_between =
        flag "-sb" (optional int)
          ~doc:
            "spaces between clocks, if using both (default \
             4)"
        (* and  *)
      in

      fun () ->
        let get_opts s =
          let list = String.split s ~on:' ' in
          let { seq; acc; _ } =
            List.find_exn sequence_data ~f:(fun x ->
                String.( = ) x.name (List.hd_exn list))
          in
          (seq, acc, int_of_string (List.hd_exn (List.tl_exn list)))
        in
            let min_seq, min_acc, min_gap = get_opts first_seq in
            match second with
            | Some secs ->
                let sec_seq, sec_acc, sec_gap = get_opts secs in
                let space =
                  match space_between with Some sb -> sb | None -> 4
                in
                Fibonacci_clock.Time.main ~space
                  [ (min_seq, min_acc, min_gap); (sec_seq, sec_acc, sec_gap) ]
            | None -> Fibonacci_clock.Time.main [ (min_seq, min_acc, min_gap) ]
                  ) *)

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command_0
