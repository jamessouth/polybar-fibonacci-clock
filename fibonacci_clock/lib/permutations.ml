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

let rec perm n res = function
  | [] -> []
  | h :: t ->
      let y = n - h in
      if y < 0 then perm n res t
      else if y > 0 then
        let res = h :: res in
        perm y res t
      else List.rev (h :: res)
