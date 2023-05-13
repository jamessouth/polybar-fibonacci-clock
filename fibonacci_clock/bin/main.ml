open Core

(* (name * [accuracy ; sequence]) *)
let sequence_data =
  [
    ("semi-fibonacci", [ 15; 1; 1; 2; 1; 3; 2; 5 ]);
    ("tetranacci-numbers", [ 15; 1; 1; 1; 1; 4; 7 ]);
    ("tribonacci-numbers", [ 15; 1; 1; 2; 4; 7 ]);
    ("padovan-numbers-15", [ 15; 1; 1; 1; 1; 2; 2; 3; 4 ]);
    ("pascals-triangle-15", [ 15; 1; 1; 1; 1; 2; 1; 1; 3; 3; 1 ]);
    ("divisors-of-928-15", [ 15; 1; 2; 4; 8 ]);
    ("fibonacci-triangle-15", [ 15; 1; 1; 1; 2; 1; 2; 3; 2; 2 ]);
    ("fibonacci-numbers", [ 20; 1; 1; 2; 3; 5; 8 ]);
    ("padovan-numbers-20", [ 20; 1; 1; 1; 1; 2; 2; 3; 4; 5 ]);
    ("pascals-triangle-20", [ 20; 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4 ]);
    ("a034298", [ 30; 1; 2; 3; 4; 6; 6; 8 ]);
    ("partition-numbers", [ 30; 1; 1; 2; 3; 5; 7; 11 ]);
    ("pascals-triangle-30", [ 30; 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4; 6; 4 ]);
    ("fibonacci-triangle-30", [ 30; 1; 1; 1; 2; 1; 2; 3; 2; 2; 3; 5; 3; 4 ]);
    ("divisors-of-928-30", [ 30; 1; 2; 4; 8; 16 ]);
    ("padovan-numbers-30", [ 30; 1; 2; 2; 3; 4; 5; 7; 9 ]);
    ("divisors-of-928-60", [ 60; 1; 2; 4; 8; 16; 29 ]);
    ("narayanas-cows", [ 60; 1; 1; 1; 2; 3; 4; 6; 9; 13; 19 ]);
    ("a331072", [ 60; 1; 2; 3; 5; 6; 8; 9; 12; 14 ]);
    ("padovan-numbers-60", [ 60; 1; 2; 2; 3; 4; 5; 7; 9; 12; 16 ]);
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
    let seq = List.tl_exn seq_find in
    let acc = List.hd_exn seq_find in

    match (List.nth_exn path 1, List.length colors) with
    | "one", 2 -> Seconds { seq; gap; acc; colors } |> main
    | "one", 4 -> Minutes { seq; gap; acc; colors } |> main
    | "both", 6 ->
        let seq_name1 = List.nth_exn path 3 in
        let seq_find1 =
          List.Assoc.find_exn sequence_data ~equal:String.( = ) seq_name1
        in
        let seq1 = List.tl_exn seq_find1 in
        let acc1 = List.hd_exn seq_find in

        Both
          ( { seq; gap; acc; colors },
            spaces,
            { seq = seq1; gap = gap1; acc = acc1; colors } )
        |> main
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
