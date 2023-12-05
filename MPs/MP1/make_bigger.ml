(* CS421 - Fall 2017
 * make_bigger.ml
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

 open Common
 
(*Problem*)
let make_bigger x y = if x > 0.0 then x+.y else if y<1.0 then y+.1.0 else y*.y;;
