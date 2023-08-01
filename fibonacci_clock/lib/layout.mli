type t = { color : int; value : int; index : int }
type time = { hour : int; minute : int; add_time : int }

val pl : ('a -> unit) -> ('a * 'a * 'a) list -> unit
val pint : (int * int * int) list -> unit
val show_add_time : int -> 'a list -> int list

val get_layout :
  time ->
  int list ->
  (int, int list) Base.List.Assoc.t option ->
  (int * int * int) list
