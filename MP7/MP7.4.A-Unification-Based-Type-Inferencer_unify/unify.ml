(* File: unify.ml *)

open Common
open Plsolution

(* PUT ANY HELPER FUNCTIONS YOU WANT UP HERE! *)


let rec unify constraints = 
  match constraints with
  |[]-> Some []
  |(s,t)::rest -> if s = t then unify rest (*Delete*)
                    else match s, t with
                    |TyConst _, TyVar _ -> unify ((t, s) :: rest)  (*Orient*)
                    |TyConst (n1, set1), TyConst (n2, set2) -> if n1 = n2 && List.length set1 = List.length set2 then (*Decompose*)
                                                              let rec collectUnionofsets l = 
                                                                match l with
                                                                |([],[])-> []
                                                                |(x1::xs1,x2::xs2)->(x1,x2) :: collectUnionofsets(xs1, xs2)
                                                                |_->[]
                                                                in unify ((collectUnionofsets (set1,set2)) @ rest )
                                                             else None
                    |TyVar svar, t -> if (occurs svar t) then None else (*Eliminate*)
                                    let typevar_ = [(svar,t)] in
                                    let rec substAll lst = 
                                      (match lst with 
                                      |[] -> []
                                      |(m1,m2)::rst ->
                                          (((monoTy_lift_subst typevar_ m1), monoTy_lift_subst typevar_ m2)) :: (substAll rst))
                                  in (match unify (substAll rest) with
                                      |None -> None
                                      |Some fi -> Some((svar, monoTy_lift_subst fi t) :: fi))

let rec canonical_form ty =
  match ty with
  | TyVar _ -> ty  (* A type variable is already in canonical form *)
  | TyConst (name, args) ->
    let canonical_args = List.map canonical_form args in
    TyConst (name, canonical_args)

let equiv_types ty1 ty2 =
  let canonical_ty1 = canonical_form ty1 in
  let canonical_ty2 = canonical_form ty2 in
  canonical_ty1 = canonical_ty2