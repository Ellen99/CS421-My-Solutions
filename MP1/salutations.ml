(* CS421 - Fall 2017
 * salutations.ml
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

 open Common
 
(*Problem*)
let salutations name = if name = "Elsa" then print_string("Halt! Who goes there!\n") else print_string("Hail, "^(name)^". We warmly welcome you!\n");;

