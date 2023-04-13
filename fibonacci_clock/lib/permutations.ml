let pl f lst =
  let rec print_elements = function
    | [] -> ()
    | h :: t ->
        f h;
        print_string ";";
        print_elements t
  in
  print_string "[";
  print_elements lst;
  print_string "]";
  print_newline ()

let pint = pl print_int
let rec sum = function [] -> 0 | h :: t -> h + sum t

(* let rec perm n res = function
   | [] -> []
   | h :: t ->
         let ores = res in let res = h :: res in let z = sum res in
         if z < n then perm n res t
         else if z > n then perm n (List.tl (List.rev ores)) (h::t)
          else res *)

let rec perm n res = function
  | [] -> []
  | h :: t ->
      let y = n - h in
      if y < 0 then perm n res t
      else if y > 0 then perm y (h :: res) t
      else (h :: res)
