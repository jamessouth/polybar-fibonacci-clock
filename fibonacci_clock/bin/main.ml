open Core

type seq = { name : string; seq : int list; acc : int }

let sequence_data =
  [
    { name = "Semi_Fibonacci"; seq = [ 1; 1; 2; 1; 3; 2; 5 ]; acc = 15 };
    { name = "Tetranacci_numbers"; seq = [ 1; 1; 1; 1; 4; 7 ]; acc = 15 };
    { name = "Tribonacci_numbers"; seq = [ 1; 1; 2; 4; 7 ]; acc = 15 };
    { name = "Padovan_numbers_15"; seq = [ 1; 1; 1; 1; 2; 2; 3; 4 ]; acc = 15 };
    {
      name = "Pascals_triangle_15";
      seq = [ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1 ];
      acc = 15;
    };
    { name = "Divisors_of_928_15"; seq = [ 1; 2; 4; 8 ]; acc = 15 };
    {
      name = "Fibonacci_triangle_15";
      seq = [ 1; 1; 1; 2; 1; 2; 3; 2; 2 ];
      acc = 15;
    };
    { name = "Fibonacci_numbers"; seq = [ 1; 1; 2; 3; 5; 8 ]; acc = 20 };
    {
      name = "Padovan_numbers_20";
      seq = [ 1; 1; 1; 1; 2; 2; 3; 4; 5 ];
      acc = 20;
    };
    {
      name = "Pascals_triangle_20";
      seq = [ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4 ];
      acc = 20;
    };
    { name = "A034298"; seq = [ 1; 2; 3; 4; 6; 6; 8 ]; acc = 30 };
    { name = "Partition_numbers"; seq = [ 1; 1; 2; 3; 5; 7; 11 ]; acc = 30 };
    {
      name = "Pascals_triangle_30";
      seq = [ 1; 1; 1; 1; 2; 1; 1; 3; 3; 1; 1; 4; 6; 4 ];
      acc = 30;
    };
    {
      name = "Fibonacci_triangle_30";
      seq = [ 1; 1; 1; 2; 1; 2; 3; 2; 2; 3; 5; 3; 4 ];
      acc = 30;
    };
    { name = "Divisors_of_928_30"; seq = [ 1; 2; 4; 8; 16 ]; acc = 30 };
    { name = "Padovan_numbers_30"; seq = [ 1; 2; 2; 3; 4; 5; 7; 9 ]; acc = 30 };
    { name = "Divisors_of_928_60"; seq = [ 1; 2; 4; 8; 16; 29 ]; acc = 60 };
    {
      name = "Narayanas_cows";
      seq = [ 1; 1; 1; 2; 3; 4; 6; 9; 13; 19 ];
      acc = 60;
    };
    { name = "A331072"; seq = [ 1; 2; 3; 5; 6; 8; 9; 12; 14 ]; acc = 60 };
    {
      name = "Padovan_numbers_60";
      seq = [ 1; 2; 2; 3; 4; 5; 7; 9; 12; 16 ];
      acc = 60;
    };
  ]

let layout =
  Command.Arg_type.create (fun opt_list ->
      match String.split opt_list ~on:' ' with
      | [ sq; gap ] ->
          let gap_error = "gap must be a positive integer or zero" in
          if
            not (List.exists sequence_data ~f:(fun x -> String.( = ) x.name sq))
          then failwith (sq ^ " is not a valid sequence option")
          else if
            not
              (try int_of_string gap > -1 with Failure _ -> failwith gap_error)
          then failwith gap_error
          else opt_list
      | [ _ ] | _ -> failwith "invalid input")



let color_list =
  Command.Arg_type.create (fun col_list ->
      match String.split col_list ~on:' ' with
      | [ _col0; _col1 ] as a -> List.map a ~f:(fun x -> if String.is_prefix x ~prefix:"#" then x else "#" ^ x)
      | [ _col0; _col1;_col2;_col3 ] as a -> List.map a ~f:(fun x -> if String.is_prefix x ~prefix:"#" then x else "#" ^ x)

      
      

      | [ _ ] | _ -> failwith "invalid input")


let command =
  Command.basic ~summary:"fib clock"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command first_seq =
       flag "-s1" (required layout) ~doc:"layout for first clock"
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



                 )

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
