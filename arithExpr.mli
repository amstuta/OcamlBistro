open Bigint

type arith_expr

val string_of_arith_expr : arith_expr   -> string
val string_of_arith_expr2: arith_expr   -> string
val print_list_expr : (arith_expr list) -> unit
val eval_expr : arith_expr -> bigint
val compile_expr : (arith_expr list) -> (char list) -> arith_expr
val solve_arith_expr : string -> bigint
