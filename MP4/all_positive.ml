open Common

(* CS421 - Fall 2016 all_positive
 * 
 * Please keep in mind that there may be more than one way to solve a
 * problem. You will want to change how a number of these start.
 *)

(*****************************
* Continuation Passing Style *
*****************************)

(* Problems *)
let rec all_positive l = match l with 
|[]-> true
|x::xs-> if x < 0 then false else all_positive xs;; 

let rec all_positivek l k = match l with
|[]-> (k true)
|x::xs-> ltk (x,0) (fun b -> if b then (k false) else (all_positivek xs k));;
