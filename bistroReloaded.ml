open Bigint
open ArithExpr

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
			       print_endline (string_of_bigint (solve_arith_expr line));
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
	(*print_endline (string_of_arith_expr2 res);*)
	print_string "Result: ";
      print_endline (string_of_bigint (solve_arith_expr line))
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
