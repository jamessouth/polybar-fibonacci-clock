open Core

let mono =
  [
    "Semi_Fibonacci";
    "Tetranacci_numbers";
    "Tribonacci_numbers";
    "Fibonacci_numbers";
    "A034298";
    "Partition_numbers";
    "Narayanas_cows";
    "A331072";
  ]

let poly =
  [
    ("Padovan_numbers_sm", [ "15"; "20" ]);
    ("Pascals_triangle", [ "15"; "20"; "30" ]);
    ("Fibonacci_triangle", [ "15"; "30" ]);
    ("Divisors_of_928", [ "15"; "30"; "60" ]);
    ("Padovan_numbers_lg", [ "30"; "60" ]);
  ]

let seq_to_ind = function "15" -> 1 | "20" -> 2 | "30" -> 3 | _ -> 4

let layout =
  Command.Arg_type.create (fun opt_list ->
      let err = "gap must be a positive integer or zero" in
      match String.split opt_list ~on:' ' with
      | [ s; a; g ] ->
          if
            not
              (List.exists poly ~f:(fun x ->
                   if String.( = ) (fst x) s then
                     List.exists (snd x) ~f:(fun y -> String.( = ) y a)
                   else false))
          then failwith (s ^ " " ^ a ^ " is not a valid sequence option")
          else if
            not (try int_of_string g > -1 with Failure _ -> failwith err)
          then failwith err
          else opt_list
      | [ s; g ] ->
          if not (List.exists mono ~f:(fun x -> String.( = ) x s)) then
            failwith (s ^ " is not a valid sequence option")
          else if
            not (try int_of_string g > -1 with Failure _ -> failwith err)
          then failwith err
          else opt_list
      | [ _ ] | _ -> failwith "improper input")

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

(* (let%map_open.Command minutes_seq =
     anon (maybe ("minutes_seq" %: seq))
   in
   fun () -> print_string minutes_seq) *)

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
