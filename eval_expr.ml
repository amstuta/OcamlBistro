type bigint = {
  value : (char list);
  sign : int }

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
 

let rec getops str idx l =
  let len = (String.length str) in
  if idx = len then (List.rev l)
  else match str.[idx] with
       | '+' | '-' | '*' | '/' | '%' -> getops str (idx + 1) (str.[idx]::l)
       | _ -> getops str (idx + 1) l


let rec get_cur_nbr str idx =
  let len = (String.length str) in
  if idx >= len then []
  else match str.[idx] with
       | '+' | '-' | '*' | '/' | '%' | '(' | ')' | ' ' -> []
       | _ -> str.[idx]::(get_cur_nbr str (idx + 1))

let rec get_next_op str idx =
  let len = (String.length str) in
  if idx >= len then idx
  else
    match str.[idx] with
    | '+' | '-' | '*' | '/' | '%' | '(' | ')' | ' ' -> idx
    | _ -> get_next_op str (idx + 1)
  

let rec get_numbers str idx l =
  let len = (String.length str) in
  if idx >= len then (List.rev l)
  else match str.[idx] with
       | '+' | '-' | '*' | '/' | '%' | '(' | ')' | ' ' -> get_numbers str (idx + 1) l
       | _ -> get_numbers str (get_next_op str idx) (( { value = (get_cur_nbr str idx) ; sign = 0 } )::l)


let rec print_list_bigint = function
  | [] -> ()
  | h::t ->
     begin
       print_string "Values: ";
       List.iter print_char h.value;
       print_endline "";
       print_string "Sign: ";
       print_int h.sign;
       print_endline "";
       print_endline "----------";
       print_list_bigint t;
     end
     

let () = print_int  (eval_expr (Mul ( (Val 2), (Sum ( (Val 1), (Val 1) ) ) ) ) );;
  print_endline "";;
  let () = List.iter print_char (getops "(1*2 +(3/3) - (6))" 0 []);;
    print_endline "";;
    let () = print_list_bigint (get_numbers "(1 * 2765 +(30/43) - (690))" 0 []);;
