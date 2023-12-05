(* CS421 - Fall 2016 all_nonneg
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
let all_nonneg l = 
  let rec neg l acm = match l with
  |[]-> acm
  |x::xs->(if x<0 then false else neg xs acm) 
  in neg l true

let all_nonneg_start = true;; (* You may need to change this *)
let all_nonneg_step r x = if x < 0 then false else r

