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

let rec print_list_int = function
  | h::t ->
     if h > 64 && h < 91 then
       begin
	 print_char (char_of_int h);
	 print_list_int t;
       end
     else
       begin
	 print_int h;
	 print_list_int t;
       end
  | []   -> ()

let convert_base from tob nbr =
  let diviseur = (String.length tob) in
  let rec cbase_in aff nb = function
    | 0 -> aff
    | _ ->
       begin
	 let quotient = nb / diviseur in
	 let reste = nb mod diviseur in
	 if reste >= 0 && reste <= 9 then
	   cbase_in (reste::aff) quotient quotient
	 else
	   cbase_in ((reste + 55)::aff) quotient quotient
       end
  in cbase_in [] nbr 1
  ;;


let a = 'e'::'a'::[];;
let b = { value = a; sign = 0 };;
  print_list b.value;;
    print_endline "";;
      
    let c = convert_base "0123456789" "01" 10;;
      print_list_int c;;
	print_endline "";;

      let d = convert_base "0123456789" "0123456789ABCDEF" 11;;
	print_list_int d;;
	  print_endline "";;
	    
	  let e = convert_base "0123456789" "01234567" 11;;
	    print_list_int e;;
	      print_endline "";;
