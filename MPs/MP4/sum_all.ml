open Common

(* CS421 - Fall 2017 sum_all
 * 
 * Please keep in mind that there may be more than one way to solve a
 * problem. You will want to change how a number of these start.
 *)

(*****************************
* Continuation Passing Style *
*****************************)

(* Problems *)
let rec sum_all (p,l) = match l with
|[]->0.0
|x::xs-> let sum =sum_all(p, xs) in (if p x then x else 0.0) +. sum;;

(*  (if (p x) then x else (0.0)) +. sum_all(p, xs) *)
 (* +. ((if (p x) then x else (0.0)) +. sum_all(p, xs)) *)
(* |x::xs-> let sum =sum_all(p, xs) in 
   (if p l then x else 0.0) +. sum;; *)

let rec sum_allk (p,l) k = match l with
|[]-> k (0.0)
|x::xs->sum_allk (p, xs) (fun v-> p x (fun px -> if px then (float_addk(x,v) k) else (float_addk((0.0),v) k)))
