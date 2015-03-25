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
  | Div(expr1, expr2) ->
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
     end
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

let rec print_list_expr = function
  | []   -> ()
  | h::t -> print_endline (print_expr_c h); print_list_expr t


							    
(* Trouve la parenthese correspondante -> substr *)
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
  in let res = find_in 1 i
     in String.sub str i res;;


(* Renvoit idx parenthese correspondante *)
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


(* Renvoit true si le char est un nombre *)
let is_number = function
  | '+' | '-' | '*' | '/' | '%' | '(' | ')' | ' ' -> false
  | _ -> true


(* NPI sur la liste d'expressions *)
let rec compile_expr nbrs ops =
  match ops with
  | []   -> List.hd nbrs
  | h::t ->
     begin
       if (List.length nbrs < 2) then raise (Failure "Invalid expression")
       else
	 let lhs = List.hd nbrs in
	 let rhs = List.hd (List.tl nbrs) in
	 let nnbrs = List.tl (List.tl nbrs) in
	 (*let nops = List.tl ops in*)
	 match h with
	 | '+' -> compile_expr (Sum (lhs, rhs)::nnbrs) t
	 | '-' -> compile_expr (Sub (lhs, rhs)::nnbrs) t
	 | '*' -> compile_expr (Mul (lhs, rhs)::nnbrs) t
	 | '/' -> compile_expr (Div (lhs, rhs)::nnbrs) t
	 | '%' -> compile_expr (Mod (lhs, rhs)::nnbrs) t
	 | _   -> compile_expr nbrs t
     end

       
(* Recupere le dernier nombre de la chaine *)
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


(* Parcourt l'expr et construit les listes d'ops & d'exprs *)
let rec feed expr =
  let rec feed_in beg idx nbrs ops =
    let len = String.length expr in
    if idx >= len then compile_expr (List.rev nbrs) (List.rev ops)
    else if (is_number expr.[idx]) = true && idx = (len - 1) then
      let nnbrs = ((Val (get_last_nbr expr))::nbrs) in
      feed_in (idx) (idx + 1) nnbrs ops
    else if (is_number expr.[idx]) = true && (is_number expr.[(idx - 1)]) = false then
      feed_in idx (idx + 1) nbrs ops
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
  else feed_in 1 1 [] []


(* Check la validite de la chaine *)
let rec check_line idx line =
  let nbrs = "0123456789abcdef" in
  let ops = "+-*/%() " in
  if idx >= (String.length line) then true
  else if (String.contains nbrs line.[idx]) = true
	  || (String.contains ops line.[idx]) = true
  then check_line (idx + 1) line
  else raise (Failure "Invalid operand")


(* Lit toutes les lignes d'un fichier *)
let rec read_file = function (fd) ->
			     try
      let line = input_line fd in
      print_endline line;
      read_file fd
    with End_of_file -> ()


(* Check l'existence du fichier *)
let check_file file =
  if (Sys.file_exists file) = false then
    begin
      print_endline "Error: file doesn't exist";
      exit 1
    end
  else
    begin
      let fd = open_in file in
      read_file fd;
      close_in fd
    end


(* Lit les exprs sur stdin *)
let read_in () =
  try
    while true do
      let line = input_line stdin in
      if line = "quit" then exit 1
      else if (check_line 0 line) = true then
	(* Enlever espaces fin *)
	let res = feed line in
	print_endline (print_expr_c res);
	print_int (eval_expr res);
	print_endline "";
    done;
  with
    End_of_file -> ()

let main =
  let argv = Array.to_list Sys.argv in
  let argc = List.length argv in
  if argc = 3 then
    check_file (List.nth argv 2)
  else
    read_in ();;

main;;
