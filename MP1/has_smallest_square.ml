(* CS421 - Fall 2017
 * has_smallest_square.ml
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

 open Common
 
(*Problem*)
let has_smallest_square m n = if m*m > n*n then n else if m*m < n*n then m else if m > n then n else m;
