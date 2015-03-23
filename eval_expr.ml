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
  | Val(value) -> string_of_int value;;


let find_sub_par str i =
  let rec find_in nb idx =
    if idx >= (String.length str) then raise (Failure "find_par")
    else match str.[idx] with
	 | ')' ->
	    begin
	      if nb = 1 then (idx - 1)
	      else find_in (nb - 1) (idx + 1)
	    end
	 | '(' -> find_in (nb + 1) (idx + 1)
	 | _   -> find_in nb (idx + 1)
  in
  let res = find_in 1 i in
  String.sub str i res;;

let find_par str i =
  let rec find_in nb idx =
    if idx >= (String.length str) then raise (Failure "find_par")
    else match str.[idx] with
	 | ')' ->
	    begin
	      if nb = 1 then (idx - 1)
	      else find_in (nb - 1) (idx + 1)
	    end
	 | '(' -> find_in (nb + 1) (idx + 1)
	 | _   -> find_in nb (idx + 1)
  in find_in 1 i

let calculate nbrs ops =
  let lhs = List.hd nbrs in
  let rhs = List.hd (List.tl nbrs) in
  let op = List.hd ops in
  let nnbrs = List.tl (List.tl nbrs) in
  match op with
  | '+' -> (Sum (lhs, rhs))::nnbrs
  | '-' -> (Sub (rhs, lhs))::nnbrs
  | '*' -> (Mul (rhs, lhs))::nnbrs
  | '/' -> (Div (rhs, lhs))::nnbrs
  | '%' -> (Mod (rhs, lhs))::nnbrs
  | _ -> nbrs


let is_number = function
  | '+' | '-' | '*' | '/' | '%' | '(' | ')' | ' ' -> false
  | _ -> true


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


let rec feed expr =
  let rec feed_in beg idx nbrs ops =
    let len = String.length expr in
    if idx >= len then compile_expr (List.rev nbrs) (List.rev ops)
    else if (is_number expr.[idx]) = true && (is_number expr.[(idx - 1)]) = false then
      begin
	if idx = (len - 1) then
	  let nnnbrs = ((Val (get_last_nbr expr))::nbrs) in
	  feed_in (idx) (idx + 1) nnnbrs ops
	else feed_in (idx) (idx + 1) nbrs ops
      end
    else if (is_number expr.[idx]) = false then
      begin
	let nnbrs = if (is_number expr.[(idx - 1)]) = true then
		      ((Val (int_of_string (String.sub expr beg (idx - beg))))::nbrs)
		    else nbrs in
	match expr.[idx] with
	| '(' ->
	   begin
	     let sub = String.sub expr idx (len - idx) in
	     let v = feed (find_sub_par sub 1) in
	     let nidx = find_par sub 1 in
	     feed_in beg (nidx + idx + 2) (v::nnbrs) ops
	   end
	| '+' | '-' | '*' | '/' | '%' -> feed_in beg (idx + 1) nnbrs (expr.[idx]::ops)
	| _   -> feed_in beg (idx + 1) nnbrs ops
      end
    else feed_in beg (idx + 1) nbrs ops
  in
  if (is_number expr.[0]) = true then feed_in 0 1 [] []
  else feed_in 1 1 [] [] ;;



let a = feed "2* (3 - 4)+(1*6*8+1)" in
    print_endline (print_expr_c a);
    let b = eval_expr a in
    print_int b;
    print_endline "";
