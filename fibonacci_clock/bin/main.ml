open Core

let sequence_data =
  [
    ("semi-fibonacci", [ 1; 1; 2; 1; 3; 2; 5 ]);
    ("tetranacci-numbers", [ 1; 1; 1; 1; 4; 7 ]);
    ("tribonacci-numbers", [ 1; 1; 2; 4; 7 ]);
    ("padovan-numbers-15", [ 1; 1; 1; 1; 2; 2; 3; 4 ]);
    ("pascals-triangle-15", [ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1 ]);
    ("divisors-of-928-15", [ 1; 2; 4; 8 ]);
    ("fibonacci-triangle-15", [ 1; 1; 1; 2; 1; 2; 3; 2; 2 ]);
    ("fibonacci-numbers", [ 1; 1; 2; 3; 5; 8 ]);
    ("padovan-numbers-20", [ 1; 1; 1; 1; 2; 2; 3; 4; 5 ]);
    ("pascals-triangle-20", [ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4 ]);
    ("a034298", [ 1; 2; 3; 4; 6; 6; 8 ]);
    ("partition-numbers", [ 1; 1; 2; 3; 5; 7; 11 ]);
    ("pascals-triangle-30", [ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4; 6; 4 ]);
    ("fibonacci-triangle-30", [ 1; 1; 1; 2; 1; 2; 3; 2; 2; 3; 5; 3; 4 ]);
    ("divisors-of-928-30", [ 1; 2; 4; 8; 16 ]);
    ("padovan-numbers-30", [ 1; 2; 2; 3; 4; 5; 7; 9 ]);
    ("divisors-of-928-60", [ 1; 2; 4; 8; 16; 29 ]);
    ("narayanas-cows", [ 1; 1; 1; 2; 3; 4; 6; 9; 13; 19 ]);
    ("a331072", [ 1; 2; 3; 5; 6; 8; 9; 12; 14 ]);
    ("padovan-numbers-60", [ 1; 2; 2; 3; 4; 5; 7; 9; 12; 16 ]);
  ]

 

  type color_list =
  | Two of string*string
  | Four of string*string* string*string
  |  Six of string*string* string*string* string*string

  type layout = 
  | Seconds of int list * int * int * color_list
  | Minutes of int list * int * int * color_list
  | Both



let parse =
  let%map_open.Command path = path
  and gap0 = flag "-g" (required int) ~doc:"int Gap for the first clock"
  and gap1 =
    flag "-gg"
      (optional_with_default 4 int)
      ~doc:"int Gap for optional second clock (default 4)"
  and spaces =
    flag "-s"
      (optional_with_default 2 int)
      ~doc:"int Spaces between clocks when using two (default 2)"
  and colors = anon (sequence ("colors" %: string)) in

  fun () ->
    List.iter path ~f:(fun x -> Stdlib.print_string (x ^ " "));

    match (List.nth_exn path 1, List.length colors) with
    | "one", 2 ->
        let gap1 = 0 and spaces = 0 in
        Stdlib.print_int gap0;
        Stdlib.print_int gap1;
        Stdlib.print_int spaces;
        let err = failwith "enter 6-digit hex colors w/wo leading '#'" in
        Stdlib.print_int
          (Stdlib.List.length
             (List.map colors ~f:(fun x ->
                  if String.is_prefix x ~prefix:"#" then
                    if String.length x = 7 then x else err
                  else if String.length x = 6 then "#" ^ x
                  else err)))
    | "one", 4 ->
        let gap1 = 0 and spaces = 0 in
        Stdlib.print_int gap0;
        Stdlib.print_int gap1;
        Stdlib.print_int spaces;
        Stdlib.print_int (Stdlib.List.length colors)
    | "both", 6 ->
        Stdlib.print_int gap0;
        Stdlib.print_int gap1;
        Stdlib.print_int spaces;
        Stdlib.print_int (Stdlib.List.length colors)
    | "one", _ -> failwith "invalid input - enter 2 or 4 colors"
    | "both", _ -> failwith "invalid input - enter 6 colors"
    | _ -> failwith "invalid input"

let subcommand_list =
  List.map sequence_data ~f:(fun x ->
      ( fst x,
        Command.basic
          ~summary:
            (List.fold (snd x) ~init:"" ~f:(fun acc y ->
                 acc ^ string_of_int y ^ " "))
          parse ))

let get_group sum rdme lst =
  Command.group ~summary:sum
    ~readme:(fun () -> rdme)
    ~preserve_subcommand_order:() lst

let () =
  Command_unix.run ~version:"1.0" ~build_info:"RWO"
    (get_group "\nA fibonacci clock for polybar" "More detailed information000"
       [
         ( "one",
           get_group "\nA fibonacci clock for polybar"
             "More detailed information111" subcommand_list );
         ( "both",
           get_group "\nA fibonacci clock for polybar"
             "More detailed information333"
             (List.map sequence_data ~f:(fun x ->
                  ( fst x,
                    get_group "" "More detailed information444" subcommand_list
                  ))) );
       ])

(* [
     ("semi-fibonacci", Command.basic ~summary:"[ 1; 1; 2; 1; 3; 2; 5 ]" parse);
     ("tetranacci-numbers", Command.basic ~summary:"[ 1; 1; 1; 1; 4; 7 ]" parse);
     ("tribonacci-numbers", Command.basic ~summary:"[ 1; 1; 2; 4; 7 ]" parse);
     ( "padovan-numbers-15",
       Command.basic ~summary:"[ 1; 1; 1; 1; 2; 2; 3; 4 ]" parse );
     ( "pascals-triangle-15",
       Command.basic ~summary:"[ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1 ]" parse );
     ("divisors-of-928-15", Command.basic ~summary:"[ 1; 2; 4; 8 ]" parse);
     ( "fibonacci-triangle-15",
       Command.basic ~summary:"[ 1; 1; 1; 2; 1; 2; 3; 2; 2 ]" parse );
     ("fibonacci-numbers", Command.basic ~summary:"[ 1; 1; 2; 3; 5; 8 ]" parse);
     ( "padovan-numbers-20",
       Command.basic ~summary:"[ 1; 1; 1; 1; 2; 2; 3; 4; 5 ]" parse );
     ( "pascals-triangle-20",
       Command.basic ~summary:"[ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4 ]" parse );
     ("a034298", Command.basic ~summary:"[ 1; 2; 3; 4; 6; 6; 8 ]" parse);
     ( "partition-numbers",
       Command.basic ~summary:"[ 1; 1; 2; 3; 5; 7; 11 ]" parse );
     ( "pascals-triangle-30",
       Command.basic ~summary:"[ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4; 6; 4 ]"
         parse );
     ( "fibonacci-triangle-30",
       Command.basic ~summary:"[ 1; 1; 1; 2; 1; 2; 3; 2; 2; 3; 5; 3; 4 ]" parse
     );
     ("divisors-of-928-30", Command.basic ~summary:"[ 1; 2; 4; 8; 16 ]" parse);
     ( "padovan-numbers-30",
       Command.basic ~summary:"[ 1; 2; 2; 3; 4; 5; 7; 9 ]" parse );
     ("divisors-of-928-60", Command.basic ~summary:"[ 1; 2; 4; 8; 16; 29 ]" parse);
     ( "narayanas-cows",
       Command.basic ~summary:"[ 1; 1; 1; 2; 3; 4; 6; 9; 13; 19 ]" parse );
     ("a331072", Command.basic ~summary:"[ 1; 2; 3; 5; 6; 8; 9; 12; 14 ]" parse);
     ( "padovan-numbers-60",
       Command.basic ~summary:"[ 1; 2; 2; 3; 4; 5; 7; 9; 12; 16 ]" parse );
   ] *)

(* [
     ("semi-fibonacci", sub_seq);
     ("tetranacci-numbers", sub_seq);
     ("tribonacci-numbers", sub_seq);
     ("padovan-numbers-15", sub_seq);
     ("pascals-triangle-15", sub_seq);
     ("divisors-of-928-15", sub_seq);
     ("fibonacci-triangle-15", sub_seq);
     ("fibonacci-numbers", sub_seq);
     ("padovan-numbers-20", sub_seq);
     ("pascals-triangle-20", sub_seq);
     ("a034298", sub_seq);
     ("partition-numbers", sub_seq);
     ("pascals-triangle-30", sub_seq);
     ("fibonacci-triangle-30", sub_seq);
     ("divisors-of-928-30", sub_seq);
     ("padovan-numbers-30", sub_seq);
     ("divisors-of-928-60", sub_seq);
     ("narayanas-cows", sub_seq);
     ("a331072", sub_seq);
     ("padovan-numbers-60", sub_seq);
   ] *)

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
                fibonacci_clock.Time.main ~space
                  [ (min_seq, min_acc, min_gap); (sec_seq, sec_acc, sec_gap) ]
            | None -> fibonacci_clock.Time.main [ (min_seq, min_acc, min_gap) ]
                  ) *)
