type bigint = {
  value : (char list);
  sign : int }

let rec print_list = function
  | h::t ->
     begin
       print_char h;
       print_list t
     end
  | []   -> ()

let convert_base from tob nbr =
  let diviseur = (String.length tob) in
  let rec cbase_in aff = function
    | 0 -> aff
    | _ ->
       begin
	 let quotient = nbr / diviseur in
	 let reste = nbr mod diviseur in
	 let nbr = quotient in
	 if reste >= 0 && reste <= 9 then
	   cbase_in reste::aff quotient
	 else
	   cbase_in (reste + 55)::aff quotient
       end
  in cbase_in [] 1
  ;;


let a = 'e'::'a'::[];;
let b = { value = a; sign = 0 };;
  print_list b.value;;
    print_endline "";;
      
      convert_base "0123456789" "01" 10;
