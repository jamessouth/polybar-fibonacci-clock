open Core

(* let list_0_to_59 = List.init ~f:(fun y -> y) 60 *)

let test_to_hour () =
  assert (
    Bool.equal
      (let res =
         List.map (List.init ~f:(fun y -> y) 24) ~f:Fibonacci_clock.Time.to_hour
       in
       List.equal Int.equal res
         ((12 :: List.init ~f:(fun y -> y + 1) 12)
         @ List.init ~f:(fun y -> y + 1) 11))
      true)

(* let test_to_min_15 () =
  let list_0_to_14 = List.init ~f:(fun y -> y) 15 in
  assert (
    Bool.equal
      (let res =
         List.map list_0_to_59 ~f:(fun x -> Fibonacci_clock.Time.to_min x 15)
       in
       List.equal Int.equal res
         (list_0_to_14 @ list_0_to_14 @ list_0_to_14 @ list_0_to_14))
      true)

let test_to_min_20 () =
  let list_0_to_19 = List.init ~f:(fun y -> y) 20 in
  assert (
    Bool.equal
      (let res =
         List.map list_0_to_59 ~f:(fun x -> Fibonacci_clock.Time.to_min x 20)
       in
       List.equal Int.equal res (list_0_to_19 @ list_0_to_19 @ list_0_to_19))
      true)

let test_to_min_30 () =
  let list_0_to_29 = List.init ~f:(fun y -> y) 30 in
  assert (
    Bool.equal
      (let res =
         List.map list_0_to_59 ~f:(fun x -> Fibonacci_clock.Time.to_min x 30)
       in
       List.equal Int.equal res (list_0_to_29 @ list_0_to_29))
      true)

let test_to_min_60 () =
  assert (
    Bool.equal
      (let res =
         List.map list_0_to_59 ~f:(fun x -> Fibonacci_clock.Time.to_min x 60)
       in
       List.equal Int.equal res list_0_to_59)
      true) *)

let test_repeat () =
  assert (
    Bool.equal
      (let res =
         List.map
           (List.init ~f:(fun y -> y) 10)
           ~f:(fun x -> Fibonacci_clock.Time.repeat "y" x)
       in
       List.equal String.equal res
         [
           "";
           "y";
           "yy";
           "yyy";
           "yyyy";
           "yyyyy";
           "yyyyyy";
           "yyyyyyy";
           "yyyyyyyy";
           "yyyyyyyyy";
         ])
      true)

let () =
  test_to_hour ();
  (* test_to_min_15 ();
  test_to_min_20 ();
  test_to_min_30 ();
  test_to_min_60 (); *)
  test_repeat ()
