val to_hour : int -> int
val to_min : int -> int -> int
val sec : int
val min : int
val hour : int
val blocks : string list
val repeat : string -> int -> string

type accuracy_level = Fifteen | Twenty | Thirty | Sixty

val acc_lvl_to_int : accuracy_level -> int

type accuracy_mode = Bars | Invert | Lines | Text

type clock = {
  seq : int list;
  gap : int;
  acc_lvl : accuracy_level;
  acc_mode : accuracy_mode;
  colors : string list;
}

type layout =
  | Seconds of clock
  | Minutes of clock
  | Both of clock * int * clock

val main : layout -> unit
