open Common

(* CS421 - Continuation Passing Style, low level functions
 * 
 * Please keep in mind that there may be more than one way to solve a
 * problem. You will want to change how a number of these start.
 *)

(*****************************
* Continuation Passing Style *
*****************************)

(* Problems *)
let consk (x, l) k = k (x::l)
let concatk (s1, s2) k = k (s1^s2)
let string_of_intk s k = k (string_of_int s)
let truncatek r k = k (truncate r)
