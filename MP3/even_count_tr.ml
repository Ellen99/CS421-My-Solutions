(* CS421 even_count_tr
 * 
 * Please keep in mind that there may be more than one way to solve a
 * problem.  You will want to change how a number of these start.
 *)

(*************************
* Patterns of Recursion *
*************************)

(*********************
 * Tail Recursion *
 *********************)

(* Problems *)
let rec even_count_tr l = 
  let rec even_cnt lst acm = match lst with
  |[]->acm
  |(x::xs) -> if x mod 2 <>0 then (even_cnt xs acm) else even_cnt xs (acm+1)
  in even_cnt l 0

let even_count_tr_start = 0 (* You may need to change this *)
let even_count_tr_step x rec_val = if x mod 2 = 0 then rec_val+1 else rec_val
