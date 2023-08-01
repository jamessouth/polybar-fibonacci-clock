open Core

type t = { color : int; value : int; index : int }
type time = { hour : int; minute : int; add_time : int }

let pl f lst =
  let rec print_elements = function
    | [] -> ()
    | h :: t ->
      let a,b,c =  h in
        Stdlib.print_string "(";
        f a;
        Stdlib.print_string ", ";
        f b;
        Stdlib.print_string ", ";
        f c;
        Stdlib.print_string "); ";

        (* f h; *)
        print_elements t
  in
  Stdlib.print_string "[";
  print_elements lst;
  Stdlib.print_string "]";
  Stdlib.print_newline ()

let pint = pl Stdlib.print_int

let show_add_time add l = 
  if add = 1 then 1::2::(List.init ~f:(fun _ -> 0) ((List.length l)-2)) else
    if add = 2 then 1::0::2::(List.init ~f:(fun _ -> 0) ((List.length l)-3)) else
      if add = 3 then 1::0::0::2::(List.init ~f:(fun _ -> 0) ((List.length l)-4)) else
         (List.init ~f:(fun _ -> 0) (List.length l)) 

let get_layout time sequence adds =
  let get_rando_seq target color_value terms =
    if target = 0 then terms
    else
      let rec inner target result term_list =
        match term_list with
        | [] -> failwith "target cannot be reached with these numbers"
        | _ ->
            let random_index =
              Random.State.int Random.State.default (List.length term_list)
            in
            let random_term = List.nth_exn term_list random_index in
            let result =
              { random_term with color = random_term.color + color_value }
              :: result
            in
            let result_sum =
              List.fold result ~init:0 ~f:(fun acc x -> acc + x.value)
            in
            let remaining_terms =
              List.filteri term_list ~f:(fun i _ -> i <> random_index)
            in
            if result_sum = target then
              List.sort (result @ remaining_terms) ~compare:(fun a b ->
                  compare a.index b.index)
            else if result_sum > target then inner target [] terms
            else inner target result remaining_terms
      in
      inner target [] terms
    in
    List.mapi sequence ~f:(fun index value -> { index; value; color = 0 })
    |> get_rando_seq time.hour 1 |> get_rando_seq time.minute 2
    |> List.map2_exn

    (
      match adds with
    | Some (a) -> 
      (match (List.Assoc.find a ~equal:(fun x y -> x = y) time.add_time) with 
      | Some (b) -> b
       | None -> show_add_time time.add_time sequence)  
    | None -> show_add_time time.add_time sequence
    )
    
    (* (List.init ~f:(fun y -> y) (List.length sequence))  *)
    
    ~f:(fun x y -> (y.color, x, y.value))






   



    (* let els = (List.take l ind) in let sum = (List.fold els ~init:0 ~f:(fun acc x -> acc + x)) in if sum = add then 
    1::(List.init ~f:(fun y -> 0) )
    else show_add_time add (succ ind) l *)











  (* in
  let ret_val t i = (t.color, i, t.value) in
  let run =
    List.mapi sequence ~f:(fun index value -> { index; value; color = 0 })
    |> get_rando_seq time.hour 1
    |> get_rando_seq time.minute 2
    |> List.mapi
  in
  match time.add_time < 2 with
  | true ->
      run ~f:(fun i x ->
          if time.add_time = 1 then
            if i < 2 then ret_val x (succ i) else ret_val x 0
          else ret_val x 0)
  | false -> (
      let eq x y = x = y in
      let first_two = List.sub sequence ~pos:0 ~len:2 in
      let one_two_list = [ 1; 2 ] in
      let map_first_two =
        run ~f:(fun i x ->
            if i = 0 then ret_val x (succ i)
            else if i = 2 then ret_val x i
            else ret_val x 0)
      in
      match time.add_time with
      | 2 ->
          if List.equal eq first_two one_two_list then
            run ~f:(fun i x ->
                if i = 1 || i = 2 then ret_val x i else ret_val x 0)
          else map_first_two
      | _ ->
          if List.equal eq (List.sub sequence ~pos:1 ~len:2) one_two_list then
            run ~f:(fun i x ->
                if i = 1 then ret_val x i
                else if i = 3 then ret_val x (pred i)
                else ret_val x 0)
          else if List.equal eq first_two one_two_list then map_first_two
          else
            run ~f:(fun i x ->
                if i = 0 then ret_val x (succ i)
                else if i = 3 then ret_val x (pred i)
                else ret_val x 0)) *)
