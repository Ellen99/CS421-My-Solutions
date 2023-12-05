(* CS421 - Fall 2016 count_element
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
let count_element l m = 
  let rec countm l m acm = match l with 
  |[]->acm
  |x::xs-> if x =m then countm xs m (1+acm) else countm xs m acm
  in countm l m 0

let count_element_start = 0;; (* You may need to change this *)
let count_element_step m acc_value x = if x = m then acc_value+1 else acc_value

