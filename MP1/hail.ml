(* CS421 - Fall 2017
 * hail.ml
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

open Common
 
(*Problem*)
let hail name = if name = "Elsa" then print_string("Wayell, hah theya, Ayelsa!") else print_string("Dear, "^(name)^". I wish you the best in CS421.\n");;