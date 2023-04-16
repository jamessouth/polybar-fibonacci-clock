type term = { color : int; value : int; index : int }

let pl f lst =
  let rec print_elements = function
    | [] -> ()
    | h :: t ->
        print_string "{c:";
        f h.color;
        print_string " v:";
        f h.value;
        print_string " i:";
        f h.index;
        print_string "} ; ";
        print_elements t
  in
  print_string "[";
  print_elements lst;
  print_string "]";
  print_newline ()

let pint = pl print_int

(* let pl2 f lst =
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

   let pint2 = pl2 print_int *)

let rec sum = function [] -> 0 | h :: t -> h.value + sum t

(* let rec sum = function [] -> 0 | h :: t -> if fst h = 1 then snd h + sum t else  sum t *)
let comp a b =
  if a.index < b.index then -1 else if a.index > b.index then 1 else 0

let ddd l = List.mapi (fun index value -> { index; value; color = 0 }) l

let get_rando_seq n t l =
  let rec inner n res ll =
    match ll with
    | [] -> failwith "n cannot be reached with these numbers"
    | _ ->
        let r = Random.int (List.length ll) in
        let randterm = List.nth ll r in
        let res =
          {
            randterm with
            color = (if t = "h" then randterm.color + 1 else randterm.color + 2);
          }
          :: res
        in
        let y = sum res in
        let nnn = List.filteri (fun i _x -> i <> r) ll in
        if y = n then List.sort comp (res @ nnn)
        else if y > n then inner n [] l
        else inner n res nnn
  in
  inner n [] l

(* let rec help = function [] -> [] | h :: t -> (0, h) :: help t *)

(* let get_rando_seq n jjj l =


   let rec inner n res ll = let () = pint2 ll in
     match ll with
     | [] ->
         if sum res < n then failwith "n cannot be reached with these numbers"
         else res
     | _ ->
         let r = Random.int (List.length ll) in let () =  print_int r in
         let res = List.mapi (fun i x -> if i = r then (1, snd x) else x) res in
         let () = pint res in
         let y = sum res in
         if y = n then res
         else if y > n then inner n jjj l
         else inner n res (List.filteri (fun i _x -> i != r) ll)
   in
   inner n jjj l *)

(* let sum arr =
     Array.fold_left (fun a b -> if fst b = 1 then a + snd b else a) 0 arr

   let get_rando_seq n l =
     let rec inner n ll =
       let gg = sum ll in
       match gg = n with
       | true -> ll
       | false ->
           if gg > n then inner n l
           else if Array.for_all (fun x -> fst x = 1) ll then
             failwith "n cannot be reached with these numbers"
           else
             let r = Random.int (Array.length ll) in
             let ll = Array.mapi (fun i x -> if i = r then (1, snd x) else x) ll in
             inner n ll
     in
     inner n l *)

(* let rec perm n res = function
   | [] -> []
   | h :: t ->
       let y = n - h in
       if y < 0 then perm n res t
       else if y > 0 then perm y (h :: res) t
       else h :: res *)
