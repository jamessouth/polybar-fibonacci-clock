open Core

(* (name * [accuracy ; sequence]) *)
(* let sequence_data =
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
   ] *)

(* let numdocs = [ ("one", "oneeeee"); ("both", "botttttt") ] *)

let accdocs =
  [
    ("bars", "zzzzz");
    ("invert", "xxxxxx");
    ("lines", "ccccc");
    ("text", "vvvvvv");
  ]

(* let seqdocs =
  [
    ("semi-fibonacci", "1 1 2 1 3 2 5");
    ("tetranacci-numbers", "1 1 1 1 4 7");
    ("tribonacci-numbers", "1 1 2 4 7");
    ("padovan-numbers-15", "1 1 1 1 2 2 3 4");
    ("pascals-triangle-15", "1 1 1 1 2 1 1 3 3 1");
    ("divisors-of-928-15", "1 2 4 8");
    ("fib-triangle-15", "1 1 1 2 1 2 3 2 2");
    ("fibonacci-numbers", "1 1 2 3 5 8");
    ("padovan-numbers-20", "1 1 1 1 2 2 3 4 5");
    ("pascals-triangle-20", "1 1 1 1 2 1 1 3 3 1 1 4");
    ("a034298", "1 2 3 4 6 6 8");
    ("partition-numbers", "1 1 2 3 5 7 11");
    ("pascals-triangle-30", "1 1 1 1 2 1 1 3 3 1 1 4 6 4");
    ("fib-triangle-30", "1 1 1 2 1 2 3 2 2 3 5 3 4");
    ("divisors-of-928-30", "1 2 4 8 16");
    ("padovan-numbers-30", "1 2 2 3 4 5 7 9");
    ("divisors-of-928-60", "1 2 4 8 16 29");
    ("narayanas-cows", "1 1 1 2 3 4 6 9 13 19");
    ("a331072", "1 2 3 5 6 8 9 12 14");
    ("padovan-numbers-60", "1 2 2 3 4 5 7 9 12 16");
  ] *)

  (* let flag_prog =
    Command.Param.(
      flag
        "prog"
        (required Filename_unix.arg_type)
        ~doc:"filename executable with statically-defined probes")
  ;; *)


  type person = { name : string list; age : int }

  let person_param  =
  let%map_open.Command path = path and
       name = flag "name" (one_or_more_as_list string) ~doc:"X name of the person"
      and age  = flag "age"  (required int)    ~doc:"N how many years old"
      in
      List.iter path ~f:(fun y -> Stdlib.print_string (y ^ " "));

      if List.length path > 1 then 
      {name; age} else {name; age}
    


  (* let rle =
    Command.Arg_type.of_map ~accept_unique_prefixes:true 
      ~case_sensitive:false
      ~list_values_in_help:true
      (String.Map.of_alist_exn [("a","aaa");("b","bbb");("c","ccc")])



  let regular_file =
    Command.Arg_type.comma_separated 
    ~allow_empty:false
      ~strip_whitespace:false
      ~unique_values:true
      rle *)

(* let number =
  let open Command.Param in *)
  (* let flag_num pre base = *)
    (* let name = sprintf "-%s" pre^base in
    let doc =
      sprintf " %s num clocks %s" base
        (List.Assoc.find_exn numdocs ~equal:String.( = ) base)
    in *)
    (* flag ~full_flag_required:() "name" person_param ~doc:"l" *)
    (* |> map ~f:(function false -> None | true -> Some base) *)
  (* in
  let pre = "num-" in
  choose_one
    [flag_num pre "both"; flag_num pre "one" ]
    ~if_nothing_chosen:Raise *)

let accuracy =
  let open Command.Param in
  let flag_acc pre base =
    let name = sprintf "-%s" pre^base in
    let doc =
      sprintf " %s acc mode %s" base
        (List.Assoc.find_exn accdocs ~equal:String.( = ) base)
    in
    flag ~full_flag_required:() name no_arg ~doc
    |> map ~f:(function false -> None | true -> Some base)
  in
  let pre = "acc-" in
  choose_one
    [
      flag_acc pre "bars";
      flag_acc pre "invert";
      flag_acc pre "lines";
      flag_acc pre "text";
    ]
    ~if_nothing_chosen:Raise

(* let seqs =
  let open Command.Param in
  let flag_seq pre base =
    let name = sprintf "-%s" pre^base in
    let doc =
      sprintf " %s %s" base
        (List.Assoc.find_exn seqdocs ~equal:String.( = ) base)
    in
    flag ~full_flag_required:() name no_arg ~doc
    |> map ~f:(function false -> None | true -> Some base)
  in
  let pre = "seq-" in
  choose_one
    [
      flag_seq pre "semi-fibonacci";
      flag_seq pre "tetranacci-numbers";
      flag_seq pre "tribonacci-numbers";
      flag_seq pre "padovan-numbers-15";
      flag_seq pre "pascals-triangle-15";
      flag_seq pre "divisors-of-928-15";
      flag_seq pre "fib-triangle-15";
      flag_seq pre "fibonacci-numbers";
      flag_seq pre "padovan-numbers-20";
      flag_seq pre "pascals-triangle-20";
      flag_seq pre "a034298";
      flag_seq pre "partition-numbers";
      flag_seq pre "pascals-triangle-30";
      flag_seq pre "fib-triangle-30";
      flag_seq pre "divisors-of-928-30";
      flag_seq pre "padovan-numbers-30";
      flag_seq pre "divisors-of-928-60";
      flag_seq pre "narayanas-cows";
      flag_seq pre "a331072";
      flag_seq pre "padovan-numbers-60";
    ]
    ~if_nothing_chosen:Raise *)

let () =
  Command_unix.run ~version:"1.0" ~build_info:"RWO"
    (Command.basic ~summary:"fib clock"
       ~readme:(fun () -> "aosihaiu")
       (let%map_open.Command num = person_param 
       and acc = accuracy
        (* and seq = seqs  *)
      in
        fun () ->
          Stdlib.print_int num.age;
          (* Stdlib.print_endline num.name; *)
          List.iter num.name ~f:(fun y -> Stdlib.print_string (y ^ " "));
          (* List.iter num ~f:(fun x -> List.iter x ~f:(fun y -> Stdlib.print_string (y ^ " "))); *)
          (* Stdlib.print_endline num; *)
          Stdlib.print_endline acc;
          (* Stdlib.print_endline seq *)
          ))
