(*let rec print_list = function
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
  | []   -> ()*)

(*
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

print_endline (string_of_bigint { value = ('0'::'1'::'2'::[]); sign = 1});

  print_list (bigint_of_string "0x10").value;;
  print_endline "";;
 *)
