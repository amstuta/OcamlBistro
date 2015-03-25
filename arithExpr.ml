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


(* NPI sur la liste d'expressions *)
let rec compile_expr nbrs ops =
  match ops with
  | []   -> List.hd nbrs
  | h::t ->
     begin
       (*
       print_endline "------------";
       List.iter print_char ops;
       print_endline "------------";*)
       if (List.length nbrs < 2) then (List.hd nbrs) (*raise (Failure "Invalid expression")*)
       else
	 let lhs = List.hd nbrs in
	 let rhs = List.hd (List.tl nbrs) in
	 let nnbrs = List.tl (List.tl nbrs) in
	 match h with
	 | '+' -> compile_expr (Sum (lhs, rhs)::nnbrs) t
	 | '*' -> compile_expr (Mul (lhs, rhs)::nnbrs) t
	 (*| '-' -> compile_expr (Sub (lhs, rhs)::nnbrs) t
	 | '/' -> compile_expr (Div (lhs, rhs)::nnbrs) t
	 | '%' -> compile_expr (Mod (lhs, rhs)::nnbrs) t*)
	 | _   -> compile_expr nbrs t
     end


let rec eval_expr = function
  | Sum(expr1, expr2) -> add (eval_expr expr1) (eval_expr expr2)
  (*| Sub(expr1, expr2) -> (eval_expr expr1) - (eval_expr expr2)*)
  | Mul(expr1, expr2) -> mul (eval_expr expr1) (eval_expr expr2)
  (*| Div(expr1, expr2) ->
     begin
       let tmp = eval_expr expr2 in
       match tmp with
       | 0 -> raise (Failure "Division par 0!")
       | _ -> (eval_expr expr1) / (eval_expr expr2)
     end
  | Mod(expr1, expr2) ->
     begin
       let tmp = eval_expr expr2 in
       match tmp with
       | 0 -> raise (Failure "Modulo par 0!")
       | _ -> (eval_expr expr1) mod (eval_expr expr2)
     end*)
  | Val(value) -> value
  

(* Renvoit idx parenthese correspondante *)
let find_par str i =
  let rec find_in nb idx =
    if idx >= (String.length str) then raise (Failure "Missing closing parenthesis")
    else match str.[idx] with
	 | ')' ->
	    begin
	      if nb = 1 then (idx - 1)
	      else find_in (nb - 1) (idx + 1)
	    end
	 | '(' -> find_in (nb + 1) (idx + 1)
	 | _   -> find_in nb (idx + 1)
  in find_in 1 i


(* Trouve la parenthese correspondante -> substr *)
let find_sub_par str i =
  let rec find_in nb idx =
    if idx >= (String.length str) then raise (Failure "Missing closing parenthesis")
    else match str.[idx] with
	 | ')' ->
	    begin
	      if nb = 1 then (idx - 1)
	      else find_in (nb - 1) (idx + 1)
	    end
	 | '(' -> find_in (nb + 1) (idx + 1)
	 | _   -> find_in nb (idx + 1)
  in let res = find_in 1 i
     in String.sub str i res
	     

(* Renvoit true si le char est un nombre *)
let is_number = function
  | '+' | '-' | '*' | '/' | '%' | '(' | ')' | ' ' -> false
  | _ -> true

let is_operator = function
  | '+' | '-' | '*' | '/' | '%' | '(' -> true
  | _ -> false


(* Recupere le dernier nombre de la chaine *)
let get_last_nbr expr =
  let len = String.length expr in
  let rec find_last idx =
    if idx = 0 then 0
    else match expr.[idx] with
	 | '+' | '-' | '*' | '/' | '%' | '(' | ')' | ' ' -> idx
	 | _ -> find_last (idx - 1)
  in let i = find_last (len - 1) in
     if expr.[i] = '-' && i > 1 && (is_operator expr.[(i - 1)]) = true then
       String.sub expr i (len - i)
     else if i = 0 && expr.[i] = '-' then
       String.sub expr i (len - i)
     else String.sub expr (i + 1) (len - i - 1)


(*
(* Parcourt l'expr et construit les listes d'ops & d'exprs *)
let rec feed expr =
  let rec feed_in beg idx nbrs ops =
    let len = String.length expr in
    if idx >= len then compile_expr (List.rev nbrs) (List.rev ops)
    else if (is_number expr.[idx]) = true && idx = (len - 1) then
      let nnbrs = ((Val (bigint_of_string (get_last_nbr expr)))::nbrs) in
      feed_in (idx) (idx + 1) nnbrs ops
    else if (is_number expr.[idx]) = true && (is_number expr.[(idx - 1)]) = false then
      feed_in idx (idx + 1) nbrs ops
    else if (is_number expr.[idx]) = false then
      begin
	let nnbrs =
	  if (is_number expr.[(idx - 1)]) = true && beg > 0 && expr.[(beg - 1)] = '-' then
	    ((Val (bigint_of_string (String.sub expr (beg - 1) (idx - beg + 1))))::nbrs)
	  else if (is_number expr.[(idx - 1)]) = true then
	    ((Val (bigint_of_string (String.sub expr beg (idx - beg))))::nbrs)
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
  else feed_in 1 1 [] []*)


let rec feed expr =
  let rec feed_in beg idx nbrs ops =
    let len = String.length expr in
    if idx >= len then compile_expr (List.rev nbrs) (List.rev ops)
    else if (is_number expr.[idx]) = true && idx = (len - 1) then
      begin
	let nnbrs = ((Val (bigint_of_string (get_last_nbr expr)))::nbrs) in
	feed_in (idx) (idx + 1) nnbrs ops
      end
    else if (is_number expr.[idx]) = true && (is_number expr.[(idx - 1)]) = false then
      feed_in idx (idx + 1) nbrs ops
    else if (is_number expr.[idx]) = false then
      begin
	let nnbrs =
	  if (is_number expr.[(idx - 1)]) = true && beg > 0 && expr.[(beg - 1)] = '-' then
	    ((Val (bigint_of_string (String.sub expr (beg - 1) (idx - beg + 1))))::nbrs)
	  else if (is_number expr.[(idx - 1)]) = true then
	    ((Val (bigint_of_string (String.sub expr beg (idx - beg))))::nbrs)
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
  else if (is_number expr.[1]) = true && expr.[0] = '-' then feed_in 0 1 [] []
  else feed_in 1 1 [] []


(* Resoud une expression *)
let solve_arith_expr expr = eval_expr (feed expr)
