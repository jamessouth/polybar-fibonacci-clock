open Core

let test_get_layout () =
  Fibonacci_clock.Layout.pint (Fibonacci_clock.Layout.get_layout {hour = 4; minute = 7;add_time = 0} [ 1; 1; 2; 1; 3; 2; 5 ] None);
  assert (1=11)




let () =
  test_get_layout ()
