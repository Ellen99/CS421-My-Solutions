open Common

(* CS421 - Fall 2016 find_all
 * 
 * Please keep in mind that there may be more than one way to solve a
 * problem. You will want to change how a number of these start.
 *)

(*****************************
* Continuation Passing Style *
*****************************)

(* Problems *)
let rec find_all (p,l) = match l with
|[]->[]
|x::xs-> if (p x) then x::(find_all (p, xs)) else (find_all (p,xs))


let rec find_allk (p,l) k = match l with
|[]-> k []
|x::xs-> (p x (fun b-> if b then find_allk (p,xs) (fun ll-> consk (x,ll) k) else  (find_allk (p,xs) k)))