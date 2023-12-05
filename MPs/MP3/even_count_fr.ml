(* CS421 - Fall 2016 even_count_fr
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

(* Problems *)
let rec even_count_fr l = match l with
|[ ]-> 0
|(x::xs)-> (if(x mod 2 = 0) then 1 else 0) + even_count_fr xs

let even_count_fr_base = 0 (* You may need to change this *)
let even_count_fr_rec r x = if r mod 2 = 0 then x+1 else x
