(* CS421
 * cpsExp
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

 open Common
 open Plsolution  (* Gives compiled solutions to earlier problems *)
 (* Leave this line here! *)
 
 let rec cps_exp e k = match e with
 |VarExp s -> VarCPS (k,s)
 |ConstExp c -> ConstCPS (k,c)
 |IfExp (e1,e2,e3) -> let v = freshFor (freeVarsInExp e2 @ freeVarsInExp e3 @ freeVarsInContCPS k)
												in cps_exp e1 (FnContCPS (v, IfCPS(v, cps_exp e2 k, cps_exp e3 k)))
 |AppExp (e1, e2) -> let v2 = freshFor(freeVarsInExp e1 @ freeVarsInContCPS k) in
											let v1 = freshFor(v2 :: freeVarsInContCPS k) in
											cps_exp e2 (FnContCPS(v2, (cps_exp e1 (FnContCPS(v1, AppCPS(k, v1, v2))))))
 |BinOpAppExp (b, e1, e2) -> let v2 = freshFor( freeVarsInExp e1 @ freeVarsInContCPS k) in
 															let v1 = freshFor( v2 :: freeVarsInContCPS k) in
															 cps_exp e2 (FnContCPS (v2, cps_exp e1 (FnContCPS(v1, BinOpAppCPS(k,b, v1, v2)))))
|MonOpAppExp (m, e) -> let v = freshFor (freeVarsInContCPS k) in
												cps_exp e (FnContCPS (v, MonOpAppCPS(k,m,v)))

|FunExp (x, e) -> FunCPS(k, x, Kvar, cps_exp e (ContVarCPS Kvar))
|LetInExp (x, e1, e2) -> cps_exp e1 (FnContCPS(x, cps_exp e2 k))
|LetRecInExp (f, x, e1, e2) ->  FixCPS(FnContCPS(f, cps_exp e2 k), f, x, Kvar, cps_exp e1 (ContVarCPS Kvar))

 (* 
 |FunExp (x, body) -> 

 |LetInExp (x, e1, e2) ->
 |LetRecInExp (f, x, e1, e2) ->  *)

(* 

 let rec cps_exp e k = match e with
 |VarExp s -> VarCPS(k,s)
 |ConstExp c -> ConstCPS(k, c)
 |IfExp (e1,e2,e3) -> let v = (freshFor (freeVarsInExp e2 @ freeVarsInExp e3 @ freeVarsInContCPS k)) in
											cps_exp e1 (FnContCPS (v, IfCPS (v, cps_exp e2 k, cps_exp e3 k)))		

 |AppExp (e1, e2) -> let freshe2 = (freshFor ( freeVarsInExp e1 @ freeVarsInContCPS k)) in
 											let freshe1 = (freshFor (freeVarsInContCPS k @ [freshe2] )) in
 												cps_exp e2 (FnContCPS(freshe2, cps_exp e1 (FnContCPS (freshe1, AppCPS(k, freshe1, freshe2 )))))
	
 |BinOpAppExp (b, e1, e2) -> let freshe2 = (freshFor ( freeVarsInExp e1 @ freeVarsInContCPS k)) in		
 															let freshe1 = (freshFor (freeVarsInContCPS k @ [freshe2])) in
															 cps_exp e2 (FnContCPS (freshe2, cps_exp e1 (FnContCPS (freshe1, BinOpAppCPS(k,b, freshe1, freshe2)))))		

 |MonOpAppExp (m, e) -> let freshe = (freshFor (freeVarsInExp e)) in
 															cps_exp e (FnContCPS (freshe, MonOpAppCPS(k,m,freshe)))
 |FunExp (x, body) -> let transformed_body = cps_exp body (ContVarCPS Kvar) in
 												FunCPS (k, x, Kvar, transformed_body)
 |LetInExp (x, e1, e2) -> cps_exp e1 (FnContCPS (x, cps_exp e2 k))
 |LetRecInExp (f, x, e1, e2) -> FixCPS(FnContCPS(f, cps_exp e2 k), f, x, Kvar, cps_exp e1 (ContVarCPS Kvar))
 *)
