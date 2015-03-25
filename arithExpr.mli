open Bigint

type arith_expr =
  | Sum of arith_expr * arith_expr
  (*| Sub of arith_expr * arith_expr*)
  | Mul of arith_expr * arith_expr
  (*| Div of arith_expr * arith_expr
  | Mod of arith_expr * arith_expr*)
  | Val of bigint


val string_of_arith_expr : arith_expr   -> string
val string_of_arith_expr2: arith_expr   -> string
val print_list_expr : (arith_expr list) -> unit
val solve_arith_expr : arith_expr -> bigint
val compile_expr : (arith_expr list) -> (char list) -> arith_expr
