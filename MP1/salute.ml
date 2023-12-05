(* CS421 - Fall 2017
 * salute.ml
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

 open Common
 
(*Problem*)
let salute name = if name = "Elsa" then print_string("What's the low-down, man?") else print_string("Hey, "^(name)^"! Give me five, man.");;
