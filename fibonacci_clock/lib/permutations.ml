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

let get_rando_seq n l =
  let rec inner n res ll =
    match ll with
    | [] ->
        if sum res < n then failwith "n cannot be reached with these numbers"
        else res
    | _ ->
        let r = Random.int (List.length ll) in
        let res = List.nth ll r :: res in
        let y = sum res in
        if y = n then res
        else if y > n then inner n [] l
        else inner n res (List.filteri (fun i _x -> i != r) ll)
  in
  inner n [] l

let rec perm n res = function
  | [] -> []
  | h :: t ->
      let y = n - h in
      if y < 0 then perm n res t
      else if y > 0 then perm y (h :: res) t
      else h :: res
