(* CS421 - Fall 2017
 * collatz
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)


(*Problem*)
let rec collatz n = if n = 1 then 0 else if n mod 2 = 0 then collatz(n/2) + 1 else collatz(n*3+1)+1;; 
