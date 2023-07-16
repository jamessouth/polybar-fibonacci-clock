open Core

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

let test_to_min_15 () =
  assert (
    Bool.equal
      (let res =
         List.map
           (List.init ~f:(fun y -> y) 60)
           ~f:(fun x -> Fibonacci_clock.Time.to_min x 15)
       in
       List.equal Int.equal res
         (List.init ~f:(fun y -> y) 15
         @ List.init ~f:(fun y -> y) 15
         @ List.init ~f:(fun y -> y) 15
         @ List.init ~f:(fun y -> y) 15))
      true)

let test_to_min_20 () =
  assert (
    Bool.equal
      (let res =
         List.map
           (List.init ~f:(fun y -> y) 60)
           ~f:(fun x -> Fibonacci_clock.Time.to_min x 20)
       in
       List.equal Int.equal res
         (List.init ~f:(fun y -> y) 20
         @ List.init ~f:(fun y -> y) 20
         @ List.init ~f:(fun y -> y) 20))
      true)

let test_to_min_30 () =
  assert (
    Bool.equal
      (let res =
         List.map
           (List.init ~f:(fun y -> y) 60)
           ~f:(fun x -> Fibonacci_clock.Time.to_min x 30)
       in
       List.equal Int.equal res
         (List.init ~f:(fun y -> y) 30 @ List.init ~f:(fun y -> y) 30))
      true)

let test_to_min_60 () =
  assert (
    Bool.equal
      (let res =
         List.map
           (List.init ~f:(fun y -> y) 60)
           ~f:(fun x -> Fibonacci_clock.Time.to_min x 60)
       in
       List.equal Int.equal res (List.init ~f:(fun y -> y) 60))
      true)

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
  test_to_min_15 ();
  test_to_min_20 ();
  test_to_min_30 ();
  test_to_min_60 ();
  test_repeat ()
