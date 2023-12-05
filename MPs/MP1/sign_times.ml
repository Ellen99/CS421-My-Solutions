(* CS421 - Fall 2017
 * sign_times
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

 open Common
 
(*Problem*)
let sign_times n m = if m*n >0 then 1 else if m*n < 0 then -1 else 0;;

