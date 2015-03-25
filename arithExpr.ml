open Bigint

type arith_expr =
  | Sum of arith_expr * arith_expr
  (*| Sub of arith_expr * arith_expr*)
  | Mul of arith_expr * arith_expr
  (*| Div of arith_expr * arith_expr
  | Mod of arith_expr * arith_expr*)
  | Val of bigint


let rec string_of_arith_expr = function
  | Sum(expr1, expr2) -> String.concat "" ["("; (string_of_arith_expr expr1); "+"; (string_of_arith_expr expr2); ")"]
  | Mul(expr1, expr2) -> String.concat "" ["("; (string_of_arith_expr expr1); "*"; (string_of_arith_expr expr2); ")"]
  (*| Sub(expr1, expr2) -> String.concat "" ["("; (string_of_arith_expr expr1); "-"; (string_of_arith_expr expr2); ")"]
  | Div(expr1, expr2) -> String.concat "" ["("; (string_of_arith_expr expr1); "/"; (string_of_arith_expr expr2); ")"]
  | Mod(expr1, expr2) -> String.concat "" ["("; (string_of_arith_expr expr1); "%"; (string_of_arith_expr expr2); ")"]*)
  | Val(value) -> string_of_bigint value


let rec string_of_arith_expr2 = function
  | Sum(expr1, expr2) -> String.concat "" ["(Sum "; (string_of_arith_expr2 expr1); ","; (string_of_arith_expr2 expr2); ")"]
  | Mul(expr1, expr2) -> String.concat "" ["(Mul "; (string_of_arith_expr2 expr1); ","; (string_of_arith_expr2 expr2); ")"]
(*  | Sub(expr1, expr2) -> String.concat "" ["(Sub "; (string_of_arith_expr2 expr1); ","; (string_of_arith_expr2 expr2); ")"]
  | Div(expr1, expr2) -> String.concat "" ["(Div "; (string_of_arith_expr2 expr1); ","; (string_of_arith_expr2 expr2); ")"]
  | Mod(expr1, expr2) -> String.concat "" ["(Mod "; (string_of_arith_expr2 expr1); ","; (string_of_arith_expr2 expr2); ")"]*)
  | Val(value) -> string_of_bigint value;;

let rec print_list_expr = function
  | []   -> ()
  | h::t -> print_endline (string_of_arith_expr2 h); print_list_expr t


let rec solve_arith_expr = function
  | Sum(expr1, expr2) -> add (solve_arith_expr expr1) (solve_arith_expr expr2)
  (*| Sub(expr1, expr2) -> (solve_arith_expr expr1) - (solve_arith_expr expr2)*)
  | Mul(expr1, expr2) -> mul (solve_arith_expr expr1) (solve_arith_expr expr2)
  (*| Div(expr1, expr2) ->
     begin
       let tmp = solve_arith_expr expr2 in
       match tmp with
       | 0 -> raise (Failure "Division par 0!")
       | _ -> (solve_arith_expr expr1) / (solve_arith_expr expr2)
     end
  | Mod(expr1, expr2) ->
     begin
       let tmp = solve_arith_expr expr2 in
       match tmp with
       | 0 -> raise (Failure "Modulo par 0!")
       | _ -> (solve_arith_expr expr1) mod (solve_arith_expr expr2)
     end*)
  | Val(value) -> value
