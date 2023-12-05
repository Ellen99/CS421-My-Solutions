(* CS421
 * freeVarsInExp
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

open Common

(* Problem 4 *)
let rec freeVarsInExp exp = match exp with
|VarExp e -> [e]
|ConstExp c-> [ ]
|MonOpAppExp (mp,e) -> freeVarsInExp e
|BinOpAppExp (b,e1,e2) -> ( (freeVarsInExp e1) @ (freeVarsInExp e2))
|IfExp (e1,e2,e3)-> ((freeVarsInExp e1) @ (freeVarsInExp e2) @ (freeVarsInExp e3))
|AppExp (e1,e2) -> ( (freeVarsInExp e1) @ (freeVarsInExp e2))
|FunExp (s,e) -> (List.filter(fun a-> not (a = s)) (freeVarsInExp e))
|LetInExp (s,e1,e2) -> (freeVarsInExp e1) @ (List.filter (fun a-> not(a=s)) (freeVarsInExp e2))  
|LetRecInExp (f,x,e1,e2) -> ((List.filter (fun a-> not( a = f || a = x))) (freeVarsInExp e1)) 
                            @ (List.filter (fun a-> not (a = f)) (freeVarsInExp e2))
                            