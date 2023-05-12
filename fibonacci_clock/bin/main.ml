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

type clock = { seq : int list; gap : int; accuracy : int; colors : string list }

type layout =
  | Seconds of clock
  | Minutes of clock
  | Both of clock * int * clock

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
