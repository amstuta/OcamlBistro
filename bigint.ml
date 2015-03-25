type bigint = { value : (char list); sign : int }
type base = Binary | Octal | Decimal | Hexadecimal


(* Met des 0 pour que l1.len == l2.len *)
let add_zeros l nb =
  let rec add_zeros_in nb =
    match nb with
    | 0   -> []
    | _   -> '0'::(add_zeros_in (nb - 1))
  in List.append (add_zeros_in nb) l
		 





(*
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
 *)

(*(* Recupere l'index du char dans la base *)
let get_index_base c = function
  | Hexadecimal -> String.index "0123456789ABCDEF" (Char.uppercase c)
  | Binary      -> String.index "01" c
  | Octal       -> String.index "01234567" c
 | Decimal     -> String.index "0123456789" c*)

		 
(* Reverse une string *)
let reverse_str s =
  let rec rev_in i =
    if i >= String.length s then "" else (rev_in (i+1))^(String.make 1 s.[i])
  in rev_in 0
				
				
(* Str -> List *)
let rec split_str str =
  match str with
  | "" -> []
  | _  -> str.[0]::(split_str(String.sub str 1 ((String.length str)-1)))
		     

(* Convertit un bigint en string *)
let string_of_bigint nbr =
  let rec build_str str = function
    | []   -> str
    | h::t -> build_str (str^(String.make 1 h)) t
  in
  if nbr.sign = 0 then build_str "" nbr.value
  else build_str "-" nbr.value;;

  
(* Addition infinie sur deux entiers non signes *)
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
  in
  let len1 = List.length b1.value in
  let len2 = List.length b2.value in
  if len1 > len2 then
    {value = (sub_add (List.rev b1.value) (List.rev (add_zeros b2.value (len1 - len2))) [] 0) ;
     sign = 0 }
  else
    {value = (sub_add (List.rev (add_zeros b1.value (len2 - len1))) (List.rev b2.value) [] 0) ;
     sign = 0 }


(* True si b1 = b2 *)
let rec compare_bigints b1 b2 =
  match (b1, b2) with
  | ([], [])       -> true
  | (h::t, hd::tl) -> if h = hd then compare_bigints t tl else false
  | _              -> false


(* Multiplication infinie *)
let mul b1 b2 =
  let res = { value = '1'::[]; sign = 0 } in
  if (compare_bigints  b1.value ('0'::[])) = true
     || (compare_bigints  b2.value ('0'::[])) = true
  then { value = '0'::[]; sign = 0}
  else
    let rec mul2 result resa = function
      | true  -> result
      | false -> mul2 (add result b2) (add resa { value = '1'::[]; sign = 0 }) (compare_bigints resa.value b1.value)
    in mul2 { value = '0'::[]; sign = 0 } res false;;

(*
let sub b1 b2 =
  if b1.sign = 0 then { value = '1'::[]; sign = 0}
  else { value = b2.value; sign = b1.sign}
	 
let div b1 b2 =
  if b1.sign = 0 then { value = '1'::[]; sign = 0}
  else { value = b2.value; sign = b1.sign}
	 
let modulo b1 b2 =
  if b1.sign = 0 then { value = '1'::[]; sign = 0}
  else { value = b2.value; sign = b1.sign}*)

(* Pow bigint nb *)
let pow nb p =
  let rec pow_rec nb p acc =
    if p = 0 then acc
    else pow_rec nb (p - 1) (mul acc nb)
  in pow_rec nb p {value = '1'::[]; sign = 0}

(* String -> bigint sans gestion du signe ou de la base *)
let get_bigint str =
  let get_sign str =
    if (String.get str 0) = '-' then
      (1, (String.sub str 1 (String.length str - 2)))
    else (0, str)
  in
  let rec convert_string nbr idx =
    if idx >= (String.length nbr) then []
    else nbr.[idx]::(convert_string nbr (idx + 1))
  in
  let (sign, nbr) = get_sign str in
  { value = (convert_string nbr 0); sign = sign}
    

(* Convertit un bigint en base 10 *)
let convert_base from nbr signe =
  let rev = reverse_str nbr in
  let res = {value = '0'::[];sign = signe} in
  let rec convert2 leni resa idx =
    if idx >= leni then resa
    else
      let now = String.index from (Char.uppercase rev.[idx]) in
      let r = get_bigint (string_of_int now) in
      let fromint = get_bigint (string_of_int (String.length from)) in
      let nbg = mul r (pow fromint idx) in
      let final = add nbg resa in
      convert2 leni final (idx + 1)
  in
  let leni = String.length rev in
  convert2 leni res 0;;


(* Convertit un string en bigint *)
let bigint_of_string str =
  let get_sign str =
    if (String.get str 0) = '-' then
      (1, (String.sub str 1 (String.length str - 1)))
    else (0, str)
  in
  let get_base str =
    if (String.get str 0) = '0' then
      match (String.get str 1) with
      | 'x' -> (Hexadecimal, (String.sub str 2 ((String.length str) - 2)))
      | 'b' -> (Binary, (String.sub str 2 ((String.length str) - 2)))
      | _   -> (Octal, (String.sub str 1 ((String.length str) - 1)))
    else (Decimal, str)
  in
  let convert_string sign base str =
    match base with
    | Hexadecimal -> convert_base "0123456789ABCDEF" str sign
    | Binary      -> convert_base "01" str sign
    | Octal       -> convert_base "01234567" str sign
    | Decimal     -> { value = (split_str str);  sign = sign}
  in
  let (sign, nbr) = get_sign str in
  let (base, nbr2)= get_base nbr in
  let a = convert_string sign base nbr2 in
  (*print_endline (string_of_bigint a);*)
  a
