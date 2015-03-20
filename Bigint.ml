type bigint = {
  value : (char list);
  sign : int }

let rec print_list = function
  | h::t ->
     begin
       print_char h;
       print_string " | ";
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

let rec list_of_string str =
  match str with
  | "" -> []
  | ch -> str.[0]::(list_of_string(String.sub str 1 ((String.length str)-1)))

let bigint_of_string str = ({value = (list_of_string str) ; sign = 0})


(* Met des 0 pour que l1.len == l2.len *)
let add_zeros l nb =
  let rec add_zeros_in nb =
    match nb with
    | 0   -> []
    | _   -> '0'::(add_zeros_in (nb - 1))
  in List.append (add_zeros_in nb) l


(* Addition infinie sur deux entiers non signes positifs *)
let add b1 b2 =
  let rec sub_add l1 l2 res ret =
    match l1 with
    | []   -> if ret = 1 then '1'::res else res
    | h::t ->
       begin
	 let v1 = (Char.code h) in
	 let v2 = (Char.code (List.hd l2)) - 48 in
	 if (v1 + v2 + ret) > 57 then
	   sub_add t (List.tl l2) ((Char.chr (v1 + v2 + ret - 10))::res) 1
	 else
	   sub_add t (List.tl l2) ((Char.chr (v1 + v2 + ret))::res) 0
       end
  in { value = (sub_add (List.rev b1.value) (List.rev b2.value) [] 0) ;
       sign = 0 }

  
let tests =
  let a = 'e'::'a'::[] in
  let b = { value = a; sign = 0 } in
  print_list b.value;
  print_endline "";
      
  let c = convert_base "0123456789" "01" 10 in
  print_list_int c;
  print_endline "";

  let d = convert_base "0123456789" "0123456789ABCDEF" 11 in
  print_list_int d;
  print_endline "";
	    
  let e = convert_base "0123456789" "01234567" 11 in
  print_list_int e;
  print_endline "";

  let g = convert_base "0123456789" "0123456789ABCDEF" 13 in
  print_list_int g;
  print_endline "";
		
  let f = bigint_of_string "12345" in
  print_string "Values: ";
  print_list f.value;
  print_endline "";
  print_string "Sign: ";
  print_int f.sign;
  print_endline "";

  let tmp2 = add_zeros ('1'::'2'::'3'::[]) 3 in
  List.iter print_char tmp2;
  print_endline "";

  (* Addition de deux bigints *)
  let b1 = { value = ('1'::'2'::'3'::[]) ; sign = 0 } in
  let b2 = { value = ('9'::'3'::'8'::[]) ; sign = 0} in
  let big = add b1 b2 in
  List.iter print_char big.value;
  print_endline "";
