type bigint = { value : (char list); sign : int }
type base = Binary | Octal | Decimal | Hexadecimal
						
val bigint_of_string : string -> bigint
val string_of_bigint : bigint -> string

val add : bigint -> bigint -> bigint
val mul : bigint -> bigint -> bigint
