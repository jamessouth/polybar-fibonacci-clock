let get_hour hr =
  match (hr < 13, hr = 0) with
  | true, true -> 12
  | true, false -> hr
  | _ -> hr - 12

let { Unix.tm_sec = sec; tm_min = min; tm_hour = hour; _ } =
  Unix.localtime (Unix.time ())



(* let blocks = [ "░"; "▒"; "▓"; "█" ] *)

(* +(4-(60/x)) *)

(* let rec take n l =
   if n = 0 then []
   else match l with [] -> [] | hd :: tl -> hd :: take (n - 1) tl *)

(* let rec flatten_pairs = function
   | [] -> []
   | (a, b) :: tl -> (a ^ b) :: flatten_pairs tl *)

(* let rec repeat_str str n =
   if n > -1 then match n with 0 -> "" | _ -> str ^ repeat_str str (n - 1)
   else "" *)


let main seq acc gap = [] 
