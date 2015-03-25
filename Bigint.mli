module type Bigint =
  sig
    type bigint
    type base
       
    val print_list : char List -> unit
    val print_list_int : int List -> unit
    val add_zeros : char List -> int -> char List
    val reverse_str : String -> String
    val get_index_base : Char -> int
    val split_str : String -> char List
    val compare_bigints : bigint -> bigint -> bool
    val pow : bigint -> int -> bigint
    val get_bigint : String -> bigint
    val convert_base : String -> String -> int -> bigint
	    
    val bigint_of_string : String -> bigint
    val string_of_bigint : bigint -> String

    val add : bigint -> bigint -> bigint
    val mul : bigint -> bigint -> bigint
  end
