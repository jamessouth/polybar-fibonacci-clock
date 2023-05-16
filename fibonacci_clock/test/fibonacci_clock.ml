let () =
  assert (
    Bool.equal
      (let res =
         List.map Fibonacci_clock.Time.to_hour (List.init 24 (fun y -> y))
       in
       List.equal Int.equal res
         ((12 :: List.init 12 (fun y -> y + 1)) @ List.init 11 (fun y -> y + 1)))
      true)
