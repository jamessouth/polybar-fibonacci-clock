(** This is a docstring, as it starts with "**", as opposed to normal
    comments that start with a single star.

    The top-most docstring of the module should contain a description
    of the module, what it does, how to use it, etc.

    The function-specific documentation located below the function
    signatures. *)

(** This is the docstring for the [greeting] function.

    A typical documentation for this function would be:

    Returns a greeting message.

    {4 Examples}

    {[ print_endline greeting ]} *)
    val time : Unix.tm
    val getHour : int -> int
    val hour : int
    val min : int
    val sec : int
    val base : string list
    val pp : int -> unit