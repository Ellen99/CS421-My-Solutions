open Common

(* CS421 - Fall 2016 even_count
 * 
 * Please keep in mind that there may be more than one way to solve a
 * problem. You will want to change how a number of these start.
 *)

(*****************************
* Continuation Passing Style *
*****************************)

(* Problems *)
let rec even_count  l   = match l with
|[]->0
(* |x::xs-> (if (x mod 2 = 0) then 1 else 0) + (even_count xs);; *)
|x::xs-> (let b = (x mod 2=0) in if b then 1 else 0) + (even_count xs);;


(* let rec even_count  l   = match l with
|[]->0
|x::xs-> let x = if (x mod 2 = 0) then 1 else 0 in x + (even_count xs);; *)

let rec even_countk l k = match l with
|[]-> k 0
|x::xs-> even_countk xs (fun v-> modk (x,2) 
                            (fun m-> eqk (m,0)
                                (fun b-> if b then (addk (1,v) k) else (addk (0,v) k))))
(* |x::xs-> modk (x,2) (fun m2-> eqk (m2,0)
                        (fun b -> even_countk xs 
                            (fun v-> if b then (addk (v,1) k) else (addk (v,0) k)))) *)

(* |x::xs->  (fun m-> eqk (m,0)
                     (fun b-> even_countk xs (fun v-> if b then (addk (v,1) k) else (k v)))) *)