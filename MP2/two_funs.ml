(* CS421 - Fall 2017
 * two_funs
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)


(*Problem*)
let two_funs f g = match (f,g) with | ((f1,f2), (p1,p2)) -> (f1 p1, f2 p2);;
