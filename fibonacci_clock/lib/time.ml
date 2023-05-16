open Core
module Unix = Core_unix

let to_hour hr =
  match (hr < 13, hr = 0) with
  | true, true -> 12
  | true, false -> hr
  | _ -> hr - 12

let to_min min acc = min - (acc * (min / acc))

let { Unix.tm_sec = sec; tm_min = min; tm_hour = hour; _ } =
  Unix.localtime (Unix.time ())

let blocks = [ "░"; "▒"; "▓"; "█" ]

(* +(4-(60/x)) *)

(* let rec flatten_pairs = function
   | [] -> []
   | (a, b) :: tl -> (a ^ b) :: flatten_pairs tl *)

let repeat s n = String.concat (List.init n ~f:(fun _ -> s))

type clock = { seq : int list; gap : int; acc : int; colors : string list }

type layout =
  | Seconds of clock
  | Minutes of clock
  | Both of clock * int * clock

let main = function
  | Seconds c -> Stdlib.print_int c.acc
  | Minutes c ->
      Stdlib.print_int hour;
      Stdlib.print_int min;
      Layout.pint
        (List.map
           ~f:(fun (col, num) ->
             List.nth_exn c.colors col
             ^ repeat (List.nth_exn blocks (min / c.acc)) num)
           (Layout.get_layout (to_hour hour) (to_min min c.acc) c.seq))
  | Both (a, b, c) ->
      Stdlib.print_int a.acc;
      Stdlib.print_int b;
      Stdlib.print_int c.acc
