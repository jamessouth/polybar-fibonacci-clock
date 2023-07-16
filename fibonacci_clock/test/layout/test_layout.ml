let test_get_layout () =
  Fibonacci_clock.Layout.pint (Fibonacci_clock.Layout.get_layout 4 7 [ 1; 1; 2; 1; 3; 2; 5 ]);
  assert (1=11)




let () =
  test_get_layout ()
