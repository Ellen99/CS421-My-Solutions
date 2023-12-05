open Common

let rec occurs (v:typeVar) (ty:monoTy) =
  match ty with
  |TyVar(i) -> if i = v then true else false
  |TyConst(tvar, args) -> 
    let rec checkargs lst =
      match lst with
      |[]-> false
      |x::xs-> if occurs v x then true else checkargs xs
    in checkargs args
    