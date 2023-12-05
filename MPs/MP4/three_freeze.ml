open Common

(* CS421 - Fall 2016 three_freeze
 * 
 * Please keep in mind that there may be more than one way to solve a
 * problem. You will want to change how a number of these start.
 *)

(*****************************
* Continuation Passing Style *
*****************************)

(* Problems *)
let three_freezek (s, p) k = concatk (s, p) (fun r -> concatk (r, r) (fun y-> concatk (r, y) k));;