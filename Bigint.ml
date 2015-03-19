open Char
open List

type bigint = {
  value : (char list);
  sign : int
}

let rec print_list = function
  | h::t ->
     begin
       print_char h;
       print_list t
     end
  | [] -> ()

let a = 'e'::'a'::[];;
let b = { value = a; sign = 0 };;
  print_list b.value;;
    print_endline "";;
