(* CS421 - Fall 2017
 * diff_square_9.ml
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

open Common
 
(*Problem*)
let diff_square_9 m n = if m < n then (n *. n -. (9.0)) else if m /.(2.0) > n then (m *. m -. (9.0)) else (m -. n)*.(m -. n) -. (9.0);;
