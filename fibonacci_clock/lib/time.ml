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

let blocks = [ "▂"; "▄"; "▆"; "█" ]

(* +(4-(60/x)) *)

(* let rec flatten_pairs = function
   | [] -> []
   | (a, b) :: tl -> (a ^ b) :: flatten_pairs tl *)

let repeat s n = String.concat (List.init n ~f:(fun _ -> s))

type accuracy_level = Fifteen | Twenty | Thirty | Sixty

let acc_lvl_to_int = function
  | Fifteen -> 15
  | Twenty -> 20
  | Thirty -> 30
  | Sixty -> 60

type accuracy_mode = Bars | Invert | Lines | Text

type clock = {
  seq : int list;
  gap : int;
  acc_lvl : accuracy_level;
  acc_mode : accuracy_mode;
  colors : string list;
}

type layout =
  | Seconds of clock
  | Minutes of clock
  | Both of clock * int * clock

let main = function
  | Seconds c -> Stdlib.print_int (acc_lvl_to_int c.acc_lvl)
  | Minutes c ->
      let g = "%{O" ^ string_of_int c.gap ^ "}" in
      Stdlib.print_string
        ("%{o#ff9900}%{+o}%{u#ff9900}%{+u}"
        ^ String.concat ~sep:g
            (List.map
               (Layout.get_layout (to_hour hour)
                  (to_min min (acc_lvl_to_int c.acc_lvl))
                  c.seq)
               ~f:(fun (col, num) ->
                 "%{F" ^ List.nth_exn c.colors col ^ "}"
                 ^ repeat
                     (List.nth_exn blocks (min / acc_lvl_to_int c.acc_lvl))
                     num)))
  | Both (a, b, c) ->
      Stdlib.print_int (acc_lvl_to_int a.acc_lvl);
      Stdlib.print_int b;
      Stdlib.print_int (acc_lvl_to_int c.acc_lvl)
