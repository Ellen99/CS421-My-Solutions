(* CS421 - Fall 2017
 * ackermann
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)


(*Problem*)
let rec naive_fibonacci n = if n<=1 then 1 else naive_fibonacci(n-1) + naive_fibonacci(n-2);;
