open Core
module Unix = Core_unix

let to_hour hr =
  match (hr < 13, hr = 0) with
  | true, true -> 12
  | true, false -> hr
  | _ -> hr - 12

let to_min min acc pt = min - (acc * pt)

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
  | Minutes {
    seq ;
    gap ;
    acc_lvl ;
    acc_mode ;
    colors 
  } ->
let acc_level = acc_lvl_to_int acc_lvl in
    let hour_part = (min / acc_level)
  in

  (* "%{o#ff9900}%{+o}%{u#ff9900}%{+u}" *)
      
      Stdlib.print_string
        (
          

         String.concat ~sep:("%{O" ^ string_of_int gap ^ "}")
            (List.map



               (Layout.get_layout 
               
               (match acc_mode with
               | Invert | Lines | Text -> {Layout.hour = (to_hour hour);

               minute = acc_level*min/60;
               
               
               adjustment = min mod (60/acc_level)
               
               }
               |  Bars -> {Layout.hour = (to_hour hour);

                minute = to_min min acc_level hour_part;
                
                
                adjustment = 0
                
                })

                  seq)



               ~f:(fun (col, num) ->
                 "%{F" ^ List.nth_exn colors col ^ "}"
                 ^ repeat

                 (
                  match acc_mode with 
                  | Bars -> List.nth_exn blocks hour_part
                  | _ -> "█"
                 )
                     
                     num))
                     
                     )



  | Both (a, b, c) ->
      Stdlib.print_int (acc_lvl_to_int a.acc_lvl);
      Stdlib.print_int b;
      Stdlib.print_int (acc_lvl_to_int c.acc_lvl)
