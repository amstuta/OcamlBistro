open ArithExpr

val find_sub_par : string -> int -> string
val find_par : string -> int -> int
val is_number : char -> bool
val get_last_nbr : string -> string
val feed : string -> arith_expr
val check_line : int -> string -> bool
val read_file : in_channel -> unit
val check_file : string -> unit
val read_in : unit -> unit
val main : unit
