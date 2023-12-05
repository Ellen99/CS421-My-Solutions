(* CS421 apply_even_odd
 * 
 * Please keep in mind that there may be more than one way to solve a
 * problem.  You will want to change how a number of these start.
 *)

(*************************
* Patterns of Recursion *
*************************)

(*********************
 * Forward Recursion *
 *********************)

(* Problem *)
let rec apply_even_odd l f g = match l with
|[]->[]
|[x]-> [f x]
|(x::y::xs) -> (f x) :: (g y) :: (apply_even_odd xs f g);;

