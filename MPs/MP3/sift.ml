(* CS421 sift
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

let rec sift p l = match l with
|[]->([],[])
|(x::xs)-> let (true_l, false_l) = sift p xs in (*destructure the result of sift recursive call*)
if p x = true then (x::true_l,false_l) else (true_l, x::false_l);;
