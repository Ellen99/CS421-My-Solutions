open Common

(* CS421 - Fall 2016 shift
 * 
 * Please keep in mind that there may be more than one way to solve a
 * problem. You will want to change how a number of these start.
 *)

(*****************************
* Continuation Passing Style *
*****************************)

(* Problems *)
let shiftk (s, q) k = float_addk (q, 1.57) (fun x1-> float_mulk (x1,x1)
                                             (fun x2 -> truncatek x2 
                                              (fun x3 -> string_of_intk x3 
                                                (fun x4-> concatk (s,x4) 
                                                  (fun x5-> concatk (x5,s) k)))))
