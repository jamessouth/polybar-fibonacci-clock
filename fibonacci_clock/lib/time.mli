module Unix = Core_unix

val to_hour : int -> int
val to_min : int -> int -> int -> int
val sec : int
val min : int
val hour : int
val repeat : string -> int -> string

type accuracy_level = Fifteen | Twenty | Thirty | Sixty

val acc_lvl_to_int : accuracy_level -> int

type accuracy_mode =
  | By_char of string list
  | By_pb_format of string list
  | Text

val acc_mode_to_list : accuracy_mode -> string list

type profile = Profile of string list

val profile_to_list : profile -> string list

type clock = {
  seq : int list;
  adds : (int * int list) list;
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
