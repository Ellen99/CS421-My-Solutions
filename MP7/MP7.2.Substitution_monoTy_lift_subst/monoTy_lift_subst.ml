open Common
open Plsolution

let rec monoTy_lift_subst (sigma: (typeVar * monoTy) list) (ty: monoTy) =
  match ty with
|TyVar(t) -> subst_fun sigma t
|TyConst(str,tvars)-> let resultargs =     
                        let rec liftvars vars = 
                            match vars with
                            |[]->[]
                            |(v::rest) -> (monoTy_lift_subst sigma v) :: liftvars rest
                            in liftvars tvars
                      in TyCnst(str, resultargs)o