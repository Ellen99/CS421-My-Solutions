(* CS421 - Fall 2017
 * welcome.ml
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

 open Common
 
(*Problem*)
let welcome name = if name = "Elsa" then print_string("Can you come out to play?\n") else print_string("Aw, come on, "^name^". We're going to have a wonderful time!\n");;
