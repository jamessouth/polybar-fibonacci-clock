val to_hour : int -> int
val to_min : int -> int -> int
val sec : int
val min : int
val hour : int
val blocks : string list
val repeat : string -> int -> string

type clock = {
  seq : int list;
  gap : int;
  acc : int;
  colors : string list;
}

type layout =
  | Seconds of clock
  | Minutes of clock
  | Both of clock * int * clock

val main : layout -> unit
