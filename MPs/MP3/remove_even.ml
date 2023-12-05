(* CS421 - Fall 2017 remove_even
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
let rec remove_even l = match l with 
|[]->[]
|(x::xs)-> (if x mod 2 = 0 then [] else [x]) @ remove_even xs;;

let remove_even_base = [] (* You may need to change this *)
let remove_even_rec n r = if n mod 2 = 0 then r else n::r;;
