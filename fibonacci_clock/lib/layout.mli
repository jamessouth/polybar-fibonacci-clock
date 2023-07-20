(* type t = { color : int; value : int; index : int }

val pl : ('a -> unit) -> ('a * 'a) list -> unit
val pint : (int * int) list -> unit
val get_layout : int -> int -> int list -> (int * int) list *)

type t = { color : int; value : int; index : int }
type time = { hour : int; minute : int; adjustment : int  }

val pl : ('a -> unit) -> ('a * 'a) list -> unit
val pint : (int * int) list -> unit
val get_layout : time -> int list -> (int * int) list

