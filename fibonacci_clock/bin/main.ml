open Core

(* (name * [accuracy ; sequence]) *)
let sequence_data =
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
  ]

let parse =
  let%map_open.Command path = path
  and gap = flag "-g" (required int) ~doc:"int Gap for the first clock"
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
    let open Fibonacci_clock.Time in
    List.iter path ~f:(fun x -> Stdlib.print_string (x ^ " "));

    let seq_name = List.nth_exn path 2 in
    let seq_find =
      List.Assoc.find_exn sequence_data ~equal:String.( = ) seq_name
    in
    let seq = fst seq_find in
    let acc = snd seq_find in
    let first = { seq; gap; acc; colors } in

    match (List.nth_exn path 1, List.length colors) with
    | "one", 2 -> Seconds first |> main
    | "one", 4 -> Minutes first |> main
    | "both", 6 ->
        let seq_name1 = List.nth_exn path 3 in
        let seq_find1 =
          List.Assoc.find_exn sequence_data ~equal:String.( = ) seq_name1
        in
        let seq1 = fst seq_find1 in
        let acc1 = snd seq_find in
        let min_colors, sec_colors = List.split_n colors 4 in

        Both
          ( { first with colors = min_colors },
            spaces,
            { seq = seq1; gap = gap1; acc = acc1; colors = sec_colors } )
        |> main
    | "one", _ ->
        failwith
          "invalid input - enter 2 or 4 colors. Escape # or quote each color."
    | "both", _ ->
        failwith "invalid input - enter 6 colors. Escape # or quote each color."
    | _ -> failwith "invalid input"

let subcommand_list =
  List.map sequence_data ~f:(fun x ->
      ( fst x,
        Command.basic
          ~summary:
            (List.fold
               (fst (snd x))
               ~init:""
               ~f:(fun acc y -> acc ^ string_of_int y ^ " "))
          ~readme:(fun () ->
            "Enter colors with leading # and escape with \\ or enclose in \
             quotes to prevent shell interpretation. Enter two colors for \
             seconds (off, on), four for minutes (off, hours, minutes, both), \
             or six for both (minutes, seconds).")
          parse ))

let get_group sum rdme lst =
  Command.group ~summary:sum
    ~readme:(fun () -> rdme)
    ~preserve_subcommand_order:() lst

let () =
  Command_unix.run ~version:"1.0" ~build_info:"RWO"
    (get_group "A Fibonacci clock for polybar" "Choose one clock or two."
       [
         ( "one",
           get_group "One clock: minutes or seconds" "Choose a sequence."
             subcommand_list );
         ( "both",
           get_group "Two clocks: minutes and seconds"
             "Choose a sequence for the minutes clock."
             (List.map sequence_data ~f:(fun x ->
                  ( fst x,
                    get_group "First clock: minutes"
                      "Choose a sequence for the seconds clock." subcommand_list
                  ))) );
       ])
