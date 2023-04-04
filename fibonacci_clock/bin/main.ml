

(* A000045  Fibonacci numbers  20
A331072                     60
A058071  Fibonacci triangle  30 15
A034298                     30
A030067  Semi-Fibonacci sequence 15
A018726  Divisors of 928  60 30 15
A007318  Pascal's triangle  30 20 15
A000931  Padovan sequence  60 30 20 15
A000930  Narayana's cows  60
A000078  Tetranacci numbers(1)  60 30 20 15
A000288  Tetranacci numbers(2)  15
A000073  Tribonacci numbers  15
A000041                      30 20  *)

open Core



let get_layout l = match l with
| "Fib_20"
| "A331072_60"
| "Fib_tri_15"
| "Fib_tri_30" -> l
| _ -> failwith "Not a valid layout option"

let minutes =
  Command.Arg_type.create get_layout

let command =
  Command.basic
    ~summary:"fib clock"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command layout =
       anon ("layout" %: minutes)
     in
     fun () -> print_string layout)

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command






(* ------------------------------------------- *)
(* adapted from https://dev.to/yawaramin/quick-and-dirty-pure-command-line-arguments-in-ocaml-3hcg *)
(* module Cmd = struct
  type t = {
    layout : string;
    minutes_gap : int;
    seconds_gap : int;
    space_between : int;
    (* repeat : int; *)
    (* msg : string  *)
  }

  let help_action () = ()

  let usage =
    "fibonacci.exe <layout> [-mg <minutes_gap>] [-sg <seconds_gap>] [-r <repeat>] <msg>\n\n\
    \  Prints a Fibonacci clock module in <layout> format (Minutes, Seconds, or Full) to polybar\n\
    \  "

  let parse () =
    let layout = ref None in
    let minutes_gap = ref 4 in
    let seconds_gap = ref 4 in
    let space_between = ref 10 in
    (* let msg = ref None in *)
    let specs =
      [
        ( "-mg",
          Arg.Set_int minutes_gap,
          "minutes_gap in pixels between numbers for the minutes clock [default 4]" );
        ( "-sg",
          Arg.Set_int seconds_gap,
          "seconds_gap in pixels between numbers for the seconds clock [default 4]" );
        ( "-sb",
          Arg.Set_int space_between,
          "space_between in pixels between the minutes and seconds displays [default 10]" );
        ("-help", Unit help_action, " Display list of options");
        ("--help", Unit help_action, " Display list of options\n");
        (* ("-r", Set_int repeat, "How many times to print the message [default 1]"); *)
      ]
    in
    let anon str = layout := Some str in
    Arg.parse specs anon usage;
    {
    layout =
      (match !layout with
      | Some l -> l
          (* match l with
        | "Minutes" | "Seconds" | "Full" -> l
        | _ -> Arg.usage specs usage;
      invalid_arg "<layout> is required: Minutes, Seconds, or Full" *)
      | None ->
          Arg.usage specs usage;
          invalid_arg "<layout> is required: Minutes, Seconds, or Full");
      minutes_gap = !minutes_gap;
      seconds_gap = !seconds_gap;
      space_between = !space_between;
    }
end 

(* let () =
   let { Cmd.gap; repeat; msg } = Cmd.parse () in
   for _ = 1 to repeat do
     Unix.sleep wait;
     print_endline msg
   done *)

  let {
    Cmd.layout;
    minutes_gap;
    seconds_gap;
    space_between
    } = Cmd.parse ()

let () = print_string layout; print_int minutes_gap; print_string " "; print_int seconds_gap; print_string " "
  
let () = Fibonacci_clock.Time.pp minutes_gap  *)


(* ------------------------------------------- *)




(* open Inifiles *)

(* let print_field ini (label, field) =
  try
    let v = ini#getval "params" field in
    Printf.printf "%s: %s\n" label v
  with Invalid_element _ ->
    Printf.printf "%s: not defined\n" label *)

(* let () = *)
  (* let ini = new inifile "../../../conf.ini"  *)
  (* let lst = [
    "Full name", "FULLNAME";
    "likes", "FAVOURITEFRUIT";
    "needs peeling", "NEEDSPEELING";
    "seeds removed", "SEEDSREMOVED";
  ] in
  List.iter (print_field ini) lst; *)

  (* let v = ini#getaval "params" "OTHERFAMILY" in
  print_endline "other family:";
  List.iter (Printf.printf "- %s\n") v; *)
;;

(* let () = Fibonacci_clock.Time.pp (ini#getval "params" "gap") *)

(* let () = print_endline (String.concat "%{O4}" Fibonacci_clock.Time.base *)

