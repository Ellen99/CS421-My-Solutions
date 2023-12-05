(* File: cmm_promote.ml *)
(* Author: Elsa L. Gunter *)
(* Fall 2023 *)
(* Share and Enjoy *)

open Common

let numeric_type_less nty1 nty2 = match (nty1,nty2) with
|(CharTy, FloatTy) -> true
|(CharTy, IntTy) -> true
|(IntTy, FloatTy) -> true
|_-> false

let max_numeric_type nty1 nty2 = if (numeric_type_less nty1 nty2) then nty2 else nty1

let promote_to_numeric_type ((ty_exp:(typed_monop, typed_binop) exp),
                             (assumed_numeric_type:numeric_type))
                              (bounding_numeric_type:numeric_type) : (typed_monop, typed_binop) exp option=

  match (assumed_numeric_type, bounding_numeric_type) with
  | (CharTy, IntTy) -> Some (MonOpAppExp (IntOfChar, ty_exp))
  | (IntTy, FloatTy) -> Some (MonOpAppExp (FloatOfInt, ty_exp))
  | (CharTy, FloatTy) -> Some (MonOpAppExp( FloatOfInt, MonOpAppExp (IntOfChar, ty_exp)))
  | (_, _) when bounding_numeric_type = assumed_numeric_type -> Some ty_exp
  | _ -> None
 
  (* val promote : (typed_monop, typed_binop) exp * exp_type -> exp_type -> (typed_monop, typed_binop) exp option = <fun> *)
let promote ((typed_exp:(typed_monop, typed_binop) exp), 
             (exp_ty:exp_type))
                          (bounding_exp_ty:exp_type) : (typed_monop, typed_binop) exp option =
  if exp_ty = bounding_exp_ty then Some typed_exp else
  match (exp_ty,bounding_exp_ty) with
  |(BasicExpTy assumed_t,BasicExpTy bounding_t) -> 
        (match (assumed_t, bounding_t) with
          |(NumTy num_assumed_t, NumTy num_bounding_t) -> promote_to_numeric_type (typed_exp, num_assumed_t) num_bounding_t
          |_-> None)
  |_-> None

