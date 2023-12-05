(* CS421 - Fall 2016 concat
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
let concat s l = 
  let rec conc s l acm = match l with 
  |[]->acm
  |[x]->acm^x
  |x::xs-> conc s xs (acm^x^s)
  in conc s l ""

