open Bigint
open ArithExpr
							    
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
     in String.sub str i res
  

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
  String.sub expr (i + 1) (len - i - 1)


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
	let nnbrs = if (is_number expr.[(idx - 1)]) = true then
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
			       print_string "Result: ";
			       print_endline (string_of_bigint (solve_arith_expr (feed line)));
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
	(*print_endline (string_of_arith_expr2 res);*)
	print_string "Result: ";
	print_endline (string_of_bigint (solve_arith_expr res));
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
