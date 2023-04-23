open Core

let padovan_15 = [ 1; 1; 1; 1; 2; 2; 3; 4 ]
let padovan_30 = [ 1; 2; 2; 3; 4; 5; 7; 9 ]
let pascal_15 = [ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1 ]
let fib_tri_15 = [ 1; 1; 1; 2; 1; 2; 3; 2; 2 ]
let div_928_15 = [ 1; 2; 4; 8 ]

let sequence_data =
  [
    ("Semi_Fibonacci", [ 1; 1; 2; 1; 3; 2; 5 ], 15);
    ("Tetranacci_numbers", [ 1; 1; 1; 1; 4; 7 ], 15);
    ("Tribonacci_numbers", [ 1; 1; 2; 4; 7 ], 15);
    ("Padovan_numbers_15", padovan_15, 15);
    ("Pascals_triangle_15", pascal_15, 15);
    ("Divisors_of_928_15", div_928_15, 15);
    ("Fibonacci_triangle_15", fib_tri_15, 15);
    ("Fibonacci_numbers", [ 1; 1; 2; 3; 5; 8 ], 20);
    ("Padovan_numbers_20", padovan_15 @ [ 5 ], 20);
    ("Pascals_triangle_20", pascal_15 @ [ 1; 4 ], 20);
    ("A034298", [ 1; 2; 3; 4; 6; 6; 8 ], 30);
    ("Partition_numbers", [ 1; 1; 2; 3; 5; 7; 11 ], 30);
    ("Pascals_triangle_30", pascal_15 @ [ 1; 4; 6; 4 ], 30);
    ("Fibonacci_triangle_30", fib_tri_15 @ [ 3; 5; 3; 4 ], 30);
    ("Divisors_of_928_30", div_928_15 @ [ 16 ], 30);
    ("Padovan_numbers_30", padovan_30, 30);
    ("Divisors_of_928_60", div_928_15 @ [ 16; 29 ], 60);
    ("Narayanas_cows", [ 1; 1; 1; 2; 3; 4; 6; 9; 13; 19 ], 60);
    ("A331072", [ 1; 2; 3; 5; 6; 8; 9; 12; 14 ], 60);
    ("Padovan_numbers_60", padovan_30 @ [ 12; 16 ], 60);
  ]

(* let seq_to_ind = function "15" -> 1 | "20" -> 2 | "30" -> 3 | _ -> 4 *)

let layout =
  Command.Arg_type.create (fun opt_list ->
      match String.split opt_list ~on:' ' with
      | [ sq; gap ] ->
          let gap_error = "gap must be a positive integer or zero" in
          if
            not
              (List.exists sequence_data ~f:(fun x ->
                   let seq, _, _ = x in
                   String.( = ) seq sq))
          then failwith (sq ^ " is not a valid sequence option")
          else if
            not
              (try int_of_string gap > -1 with Failure _ -> failwith gap_error)
          then failwith gap_error
          else opt_list
      | [ _ ] | _ -> failwith "invalid input")

let command =
  Command.basic ~summary:"fib clock"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command minutes =
       flag "-mins" (optional layout) ~doc:"layout for minutes clock"
     and seconds =
       flag "-secs" (optional layout) ~doc:"layout for seconds clock"
     and space_between =
       flag "-sb" (optional int)
         ~doc:
           "spaces between minutes and seconds clocks, if using both (default \
            4)"
       (* and  *)
     in

     fun () ->
       match minutes with
       | Some mins -> (
           match seconds with
           | Some secs -> (
               match space_between with
               | Some sb -> print_endline (mins ^ secs ^ string_of_int sb)
               | None -> print_endline (mins ^ secs ^ string_of_int 4))
           | None -> print_endline mins)
       | None -> (
           match seconds with
           | Some secs -> print_endline secs
           | None ->
               failwith "a minutes or seconds layout, or both, must be provided"
           ))

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
