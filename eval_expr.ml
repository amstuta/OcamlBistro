type expression =
  | Sum of expression * expression
  | Sub of expression * expression
  | Mul of expression * expression
  | Div of expression * expression
  | Mod of expression * expression
  | Val of int;;

let rec eval_expr = function
  | Sum(expr1, expr2) -> (eval_expr expr1) + (eval_expr expr2)
  | Sub(expr1, expr2) -> (eval_expr expr1) - (eval_expr expr2)
  | Mul(expr1, expr2) -> (eval_expr expr1) * (eval_expr expr2)
  | Div(expr1, expr2) -> (eval_expr expr1) / (eval_expr expr2)
  | Mod(expr1, expr2) -> (eval_expr expr1) mod (eval_expr expr2)
  | Val(value) -> value;;

let rec compile_expr = function
  | Sum(expr1, expr2) -> String.concat "" ["("; (compile_expr expr1); "+"; (compile_expr expr2); ")"]
  | Sub(expr1, expr2) -> String.concat "" ["("; (compile_expr expr1); "-"; (compile_expr expr2); ")"]
  | Mul(expr1, expr2) -> String.concat "" ["("; (compile_expr expr1); "*"; (compile_expr expr2); ")"]
  | Div(expr1, expr2) -> String.concat "" ["("; (compile_expr expr1); "/"; (compile_expr expr2); ")"]
  | Mod(expr1, expr2) -> String.concat "" ["("; (compile_expr expr1); "%"; (compile_expr expr2); ")"]
  | Val(value) -> string_of_int value


let () = print_int  (eval_expr (Mul ( (Val 2), (Sum ( (Val 1), (Val 1) ) ) ) ) );;
  print_endline "";;
let () = print_endline (compile_expr (Mul ( (Val 2), (Sum ( (Val 1), (Val 1) ) ) ) ));;
