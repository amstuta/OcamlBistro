let rec compare_bigints b1 b2 =
  match (b1, b2) with
  | (h::t, hd::tl) -> if h = hd then compare_bigints t tl else false
  | ([], [])       -> true
  | _              -> false
