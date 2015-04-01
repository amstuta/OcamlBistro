open Bigint
open ArithExpr

(* Check la validite de la chaine *)
let rec check_line idx line =
  let nbrs = "0123456789abcdefx" in
  let ops = "+-*/%() " in
  if idx >= (String.length line) then true
  else if (String.contains nbrs line.[idx]) = true
	|| (String.contains nbrs (Char.lowercase line.[idx]))
	|| (String.contains ops line.[idx]) = true
  then check_line (idx + 1) line
  else raise (Failure "Invalid operand")


(* Chekc si que des espaces sur la ligne *)
let rec only_spaces idx line =
  if idx >= (String.length line) then true
  else match line.[idx] with
       | ' ' | '\t' -> only_spaces (idx + 1) line
       | _ -> false


(* Lit un fichier ligne par ligne *)
let rec read_file fd =
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
let read_in =
  try
    while true do
      let line = input_line stdin in
      if line = "quit" then exit 1
      else if (only_spaces 0 line) = false && (check_line 0 line) = true then
	(* Enlever espaces fin *)
	(*print_endline (string_of_arith_expr2 res);*)
	begin
	  print_string "Result: ";
	  print_endline (string_of_bigint (solve_arith_expr line))
	end
    done;
  with
    End_of_file -> ()


let main =
  let argc = Array.length Sys.argv in
  match argc with
  | 1 -> read_in
  | 2 -> check_file (Array.get Sys.argv 2)
  | 3 -> if (Array.get Sys.argv 2) <> "-obase" then
	   raise (Invalid_argument "Wrong argument")
	 else read_in
  | 4 -> if (Array.get Sys.argv 2) <> "-obase" then
	   raise (Invalid_argument "Wrong argument")
	 else check_file (Array.get Sys.argv 3)
  | _ -> raise (Invalid_argument "Usage: ./bistro [-obase (2|8|10|16)] [inputfile]")

let _ = main
