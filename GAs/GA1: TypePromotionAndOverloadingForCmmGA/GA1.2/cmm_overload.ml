(* File: cmm_overload.ml *)
(* Author: Elsa L. Gunter *)
(* Spring 2023 *)
(* Share and Enjoy *)

open Common
open Plsolution


(* | (CharTy, IntTy) -> Some (MonOpAppExp (IntOfChar, ty_exp))
| (IntTy, FloatTy) -> Some (MonOpAppExp (FloatOfInt, ty_exp))
| (CharTy, FloatTy) -> Some (MonOpAppExp( FloatOfInt, MonOpAppExp (IntOfChar, ty_exp))) *)

let overload_binop (untyped_binop:untyped_binop) (bty:numeric_type) : (typed_binop * numeric_type) option =
match bty with
|CharTy -> (match untyped_binop with
            |PlusOp -> Some (IntPlusOp, IntTy)
            |MinusOp -> Some (IntMinusOp, IntTy)
            |TimesOp -> Some (IntTimesOp, IntTy)
            |DivOp -> Some (IntDivOp, IntTy)
            |ModOp -> Some (IntModOp, IntTy)
            |_-> None)
|IntTy -> (match untyped_binop with
            |PlusOp -> Some (IntPlusOp, bty)
            |MinusOp -> Some (IntMinusOp, bty)
            |TimesOp -> Some (IntTimesOp, bty)
            |DivOp -> Some (IntDivOp, bty)
            |ModOp -> Some (IntModOp, bty)
            |_-> None)
|FloatTy -> (match untyped_binop with
            |PlusOp -> Some (FloatPlusOp, bty)
            |MinusOp -> Some (FloatMinusOp, bty)
            |TimesOp -> Some (FloatTimesOp, bty)
            |DivOp -> Some (FloatDivOp, bty)
            |_-> None)

let overload_monop (untyped_monop:untyped_monop) (bty:numeric_type) :(typed_monop * numeric_type) option = 
match bty with
|CharTy -> (match untyped_monop with
            |NegOp -> Some (IntNegOp, IntTy)
            |_-> None)
|IntTy -> (match untyped_monop with
            |NegOp -> Some (IntNegOp, bty)
            |_-> None)
|FloatTy -> (match untyped_monop with
            |NegOp -> Some (FloatNegOp, bty)
            |_-> None)
|_->None