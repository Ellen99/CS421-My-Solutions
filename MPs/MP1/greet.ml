(* CS421 - Fall 2017
 * greet.ml
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

 open Common
 
(*Problem*)
let greet name = if name = "Elsa" then print_string("Hey Elsa, cool man!") else print_string("Hello, "^(name)^ ". I hope you enjoy CS421.\n");;
