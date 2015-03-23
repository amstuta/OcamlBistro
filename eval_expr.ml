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



let is_number = function
  | '+' | '-' | '*' | '/' | '%' | '(' | ')' | ' ' -> false
  | _ -> true

	   
let rec print_expr = function
  | Sum(expr1, expr2) -> String.concat "" ["("; (print_expr expr1); "+"; (print_expr expr2); ")"]
  | Sub(expr1, expr2) -> String.concat "" ["("; (print_expr expr1); "-"; (print_expr expr2); ")"]
  | Mul(expr1, expr2) -> String.concat "" ["("; (print_expr expr1); "*"; (print_expr expr2); ")"]
  | Div(expr1, expr2) -> String.concat "" ["("; (print_expr expr1); "/"; (print_expr expr2); ")"]
  | Mod(expr1, expr2) -> String.concat "" ["("; (print_expr expr1); "%"; (print_expr expr2); ")"]
  | Val(value) -> string_of_int value


let rec print_expr_c = function
  | Sum(expr1, expr2) -> String.concat "" ["(Sum "; (print_expr_c expr1); ","; (print_expr_c expr2); ")"]
  | Sub(expr1, expr2) -> String.concat "" ["(Sub "; (print_expr_c expr1); ","; (print_expr_c expr2); ")"]
  | Mul(expr1, expr2) -> String.concat "" ["(Mul "; (print_expr_c expr1); ","; (print_expr_c expr2); ")"]
  | Div(expr1, expr2) -> String.concat "" ["(Div "; (print_expr_c expr1); ","; (print_expr_c expr2); ")"]
  | Mod(expr1, expr2) -> String.concat "" ["(Mod "; (print_expr_c expr1); ","; (print_expr_c expr2); ")"]
  | Val(value) -> string_of_int value


let calculate nbrs ops =
  let lhs = List.hd nbrs in
  let rhs = List.hd (List.tl nbrs) in
  let op = List.hd ops in
  let nnbrs = List.tl (List.tl nbrs) in
  print_string "Lhs: ";
  print_endline (print_expr lhs);
  print_string "Rhs: ";
  print_endline (print_expr rhs);
  print_string "Op: ";
  print_char op;
  print_endline "";
  match op with
  | '+' -> (Sum (lhs, rhs))::nnbrs
  | '-' -> (Sub (rhs, lhs))::nnbrs
  | '*' -> (Mul (rhs, lhs))::nnbrs
  | '/' -> (Div (rhs, lhs))::nnbrs
  | '%' -> (Mod (rhs, lhs))::nnbrs
  | _ -> nbrs


let rec print_list_expr = function
  | [] -> ()
  | h::t -> print_string (print_expr_c h); print_string " "; print_list_expr t


let rec compile_expr nbrs ops =
  match ops with
  | []   -> List.hd nbrs
  | h::t ->
     begin
       let lhs = List.hd nbrs in
       let rhs = List.hd (List.tl nbrs) in
       let nnbrs = List.tl (List.tl nbrs) in
       let nops = List.tl ops in
       match h with
	 (*
       | '+' -> compile_expr (Sum (lhs, rhs)::nnbrs) nops
       | '-' -> compile_expr (Sub (rhs, lhs)::nnbrs) nops
       | '*' -> compile_expr (Mul (rhs, lhs)::nnbrs) nops
       | '/' -> compile_expr (Div (rhs, lhs)::nnbrs) nops
       | '%' -> compile_expr (Mod (rhs, lhs)::nnbrs) nops*)
       | '+' -> compile_expr (Sum (lhs, rhs)::nnbrs) nops
       | '-' -> compile_expr (Sub (lhs, rhs)::nnbrs) nops
       | '*' -> compile_expr (Mul (lhs, rhs)::nnbrs) nops
       | '/' -> compile_expr (Div (lhs, rhs)::nnbrs) nops
       | '%' -> compile_expr (Mod (lhs, rhs)::nnbrs) nops
       | _   -> compile_expr nbrs nops
     end
			  

let get_last_nbr expr =
  let len = String.length expr in
  let rec find_last idx =
    if idx = 0 then 0
    else match expr.[idx] with
	 | '+' | '-' | '*' | '/' | '%' | '(' | ')' | ' ' -> idx
	 | _ -> find_last (idx - 1)
  in
  let i = find_last (len - 1) in
  int_of_string (String.sub expr (i + 1) (len - i - 1))


let feed expr =
  let rec feed_in beg idx nbrs ops =
    let len = String.length expr in
    if idx >= len then compile_expr (List.rev nbrs) (List.rev ops)
    else if (is_number expr.[idx]) = true && (is_number expr.[(idx - 1)]) = false then
      begin
	if idx = (len - 1) then
	  let nnnbrs = ((Val (get_last_nbr expr))::nbrs) in
	  feed_in (idx) (idx + 1) nnnbrs ops
	else
	  feed_in (idx) (idx + 1) nbrs ops
      end
    else if (is_number expr.[idx]) = false then
      begin
	let nnbrs = if (is_number expr.[(idx - 1)]) = true then
		      begin
			let tmp = String.sub expr beg (idx - beg) in
			((Val (int_of_string tmp))::nbrs)
		      end
		    else nbrs in
	match expr.[idx] with
	| ')' -> feed_in beg (idx + 1) (calculate nnbrs ops) (List.tl ops)
	| '+' -> feed_in beg (idx + 1) nnbrs ('+'::ops)
	| '-' -> feed_in beg (idx + 1) nnbrs ('-'::ops)
	| '*' -> feed_in beg (idx + 1) nnbrs ('*'::ops)
	| '/' -> feed_in beg (idx + 1) nnbrs ('/'::ops)
	| '%' -> feed_in beg (idx + 1) nnbrs ('%'::ops)
	| _   -> feed_in beg (idx + 1) nnbrs ops
      end
    else feed_in beg (idx + 1) nbrs ops
  in
  if (is_number expr.[0]) = true then feed_in 0 1 [] []
  else feed_in 1 1 [] []


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
  let () = List.iter print_char (getops "1*2 +(3/3) - 6" 0 []);;
    print_endline "";;
    let () = print_list_bigint (get_numbers "(1 * 2765 +(30/43) - (690))" 0 []);;
    let a = feed "(2*2)+(3/3) - 6+(9-6)-(2*3)" in
	let str = print_expr_c a in
	print_endline str;
	print_endline (print_expr a);
	let b = eval_expr a in
	print_int b;;
