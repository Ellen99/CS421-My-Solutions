(* File: cmm_typed_exp_of_untyped_exp.ml *)
(* Author: Elsa L. Gunter *)
(* Spring 2023 *)
(* Share and Enjoy *)

open Common
open Plsolution

let const_type (const:const) : exp_type = 
match const with
| BoolConst _->  BasicExpTy BoolTy
| CharConst _ -> BasicExpTy (NumTy CharTy)
| IntConst _ -> BasicExpTy (NumTy IntTy)
| FloatConst _ -> BasicExpTy (NumTy FloatTy)

let rec typed_exp_of_untyped_exp (env:(string * declarable_type) list)
                                 (untyped_exp:(untyped_monop, untyped_binop) exp):
                                 ((typed_monop, typed_binop) exp * exp_type) option = 
 match untyped_exp with
|ConstExp c -> Some (ConstExp c, const_type c)
|IdentExp s-> (match lookup_env env s with
               |Some VarTy (ArrayVarTy _) -> None
               |Some VarTy vtype-> Some (IdentExp s, exp_type_of_var_type vtype)
               |_->None)

|ArrayEltExp (s, e) -> (match lookup_env env s with
                        |Some (VarTy (ArrayVarTy elem_type))-> 
                            (match typed_exp_of_untyped_exp env e with
                             |Some (translated_e, translated_exp_type) -> 
                                (match promote (translated_e, translated_exp_type) (BasicExpTy (NumTy IntTy)) with
                                 |Some typed_exp -> Some ((ArrayEltExp (s, typed_exp), BasicExpTy elem_type))
                                 |_->None)
                            |_->None)
                         |_-> None)

| MonOpAppExp (monop, e) -> 
  (match monop with
    |NegOp -> (match typed_exp_of_untyped_exp env e with
              |Some (translated_arg, (BasicExpTy (NumTy translated_arg_type))) -> 
                  (match overload_monop monop translated_arg_type with
                    |Some (typed_monop, req_arg_num_type) -> 
                        (match promote (translated_arg, (BasicExpTy (NumTy translated_arg_type))) (BasicExpTy (NumTy req_arg_num_type)) with
                        |Some promoted_exp -> Some(MonOpAppExp (typed_monop, promoted_exp), BasicExpTy (NumTy req_arg_num_type))
                        |_->None)
                    |_->  None)
              |_->  None)
    |NotOp -> (match typed_exp_of_untyped_exp env e with
              |Some (translated_arg, (BasicExpTy (BoolTy ))) -> 
                  (match monop with
                  |NotOp -> Some(MonOpAppExp (BoolNotOp, translated_arg), BasicExpTy BoolTy)
                  |_->None)
              |_->  None))

| BinOpAppExp (binop, exp1 ,exp2) -> 
  (match binop with
    |ConcatOp -> (match (typed_exp_of_untyped_exp env exp1, typed_exp_of_untyped_exp env exp2) with
                    |(Some (translated_arg1, (ArrayVarExpTy (NumTy CharTy ))),
                      Some (translated_arg2, (BasicExpTy (NumTy CharTy)))) ->
                      (* maximum of type is max_numeric_type *)  
                        (match overload_binop binop (max_numeric_type CharTy CharTy) with
                        | Some (typed_binop, req_arg_type) -> 
                          (match (promote (translated_arg1, BasicExpTy (NumTy CharTy)) (BasicExpTy (NumTy req_arg_type)),
                            promote (translated_arg2, BasicExpTy (NumTy CharTy)) (BasicExpTy (NumTy req_arg_type))) with
                          |(Some promoted_arg1, Some promoted_arg2) -> 
                            Some (BinOpAppExp (typed_binop, promoted_arg1,promoted_arg2), (BasicExpTy (NumTy req_arg_type))) 
                          |_->None
                          )
                        |_->None
                        ) 
                    |_->None)
    |ModOp -> (match (typed_exp_of_untyped_exp env exp1, typed_exp_of_untyped_exp env exp2) with
              |(Some (translated_arg1, (BasicExpTy (NumTy IntTy))),
                Some (translated_arg2, (BasicExpTy (NumTy IntTy)))) ->
                (* maximum of type is max_numeric_type *)  
                  (match overload_binop binop (max_numeric_type IntTy IntTy) with
                  | Some (typed_binop, req_arg_type) -> 
                    (match (promote (translated_arg1, BasicExpTy (NumTy IntTy)) (BasicExpTy (NumTy req_arg_type)),
                      promote (translated_arg2, BasicExpTy (NumTy IntTy)) (BasicExpTy (NumTy req_arg_type))) with
                    |(Some promoted_arg1, Some promoted_arg2) -> 
                      Some (BinOpAppExp (typed_binop, promoted_arg1,promoted_arg2), (BasicExpTy (NumTy req_arg_type))) 
                    |_->None
                    )
                  |_->None
                  ) 
              |_->None)

    |PlusOp | MinusOp | TimesOp | DivOp->
        (match (typed_exp_of_untyped_exp env exp1, typed_exp_of_untyped_exp env exp2) with
          |(Some (translated_arg1, (BasicExpTy (NumTy translated_arg1_type))),
            Some (translated_arg2, (BasicExpTy (NumTy translated_arg2_type)))) ->
            (* maximum of type is max_numeric_type *)  
              (match overload_binop binop (max_numeric_type translated_arg1_type translated_arg2_type) with
              | Some (typed_binop, req_arg_type) -> 
                (match (promote (translated_arg1, BasicExpTy (NumTy translated_arg1_type)) (BasicExpTy (NumTy req_arg_type)),
                  promote (translated_arg2, BasicExpTy (NumTy translated_arg2_type)) (BasicExpTy (NumTy req_arg_type))) with
                |(Some promoted_arg1, Some promoted_arg2) -> 
                  Some (BinOpAppExp (typed_binop, promoted_arg1,promoted_arg2), (BasicExpTy (NumTy req_arg_type))) 
                |_->None
                )
              |_->None
              ) 
          |_->None))
(* If the expression is the application of a monadic, binary or relational operator,
   first translate the arguments,
    then find the maximum of the types of the arguments, 
    find the typed operator among the overloaded variants of the given untyped operator 
    with the least input type to which that maximum can be promoted, 
    promote all arguments to that type,
     apply the typed version of the operator to the promoted arguements to get the needed typed expression, 
     which then is the translation of the given untyped expression,
      and return that typed expression paired with the output type of the operator. 
      (Note that strings are represented as arrays containing char elements.)
 *)
(* type rel_op = EqOp | NotEqOp | LessOp | LessEqOp | GreaterOp | GreaterEqOp *)
| RelOpAppExp (rel_op, exp1, exp2)->
  (match (typed_exp_of_untyped_exp env exp1, typed_exp_of_untyped_exp env exp2) with
    |(Some (translated_arg1, (BasicExpTy (NumTy translated_arg1_type))),
      Some (translated_arg2, (BasicExpTy (NumTy translated_arg2_type)))) ->
        let maxType = max_numeric_type translated_arg1_type translated_arg2_type in
        (match (promote (translated_arg1, BasicExpTy (NumTy translated_arg1_type)) (BasicExpTy (NumTy maxType)),
                promote (translated_arg2, BasicExpTy (NumTy translated_arg2_type)) (BasicExpTy (NumTy maxType))) with
          |(Some promoted_arg1, Some promoted_arg2) -> 
            Some (RelOpAppExp (rel_op, promoted_arg1, promoted_arg2), (BasicExpTy BoolTy))  
          |_->None)
    |(Some (translated_arg1, (BasicExpTy BoolTy)),
      Some (translated_arg2, (BasicExpTy BoolTy))) ->
            Some (RelOpAppExp (rel_op, translated_arg1, translated_arg2), (BasicExpTy BoolTy))  
          |_->None)
(* If the expression is the application of a logical operator, i.e. && or ||,
 translate the arguments, check that they have boolean type, 
 reapply the logical operator to the typed versions of the arguments to get the typed translation of the original expression,
  and return this typed expression paired with the type of booleans.
 *)
| AndExp ( exp1,exp2) -> (match (typed_exp_of_untyped_exp env exp1, typed_exp_of_untyped_exp env exp2) with
                          |(Some (translated_arg1, (BasicExpTy BoolTy)),
                            Some (translated_arg2, (BasicExpTy BoolTy))) ->
                             Some (AndExp (translated_arg1, translated_arg2), (BasicExpTy BoolTy))  
                             |_->None)
                              
| OrExp ( exp1,exp2) -> (match (typed_exp_of_untyped_exp env exp1, typed_exp_of_untyped_exp env exp2) with
                          |(Some (translated_arg1, (BasicExpTy BoolTy)),
                            Some (translated_arg2, (BasicExpTy BoolTy))) ->
                             Some (OrExp (translated_arg1, translated_arg2), (BasicExpTy BoolTy))  
                             |_->None)

(* VarTy of var_type
  | FunTy of function_return_type * var_type list *)
| CallExp (s, explist) -> (match lookup_env env s with
                            |Some (FunTy (f_return_type, varTypeslst)) -> 
                              let rec applyArgs args argtypes =
                                 (match (args,argtypes) with
                                  |([],[])-> Some ([])
                                  |((argi::rest), (argi_type::rest_types)) -> 
                                      (match typed_exp_of_untyped_exp env argi with
                                      |Some (translated_argi, (BasicExpTy (NumTy translated_argi_type))) ->
                                        (match promote (translated_argi, (BasicExpTy (NumTy translated_argi_type))) (exp_type_of_var_type argi_type) with
                                        |Some promoted_typei -> (match  (applyArgs rest rest_types) with
                                                                |Some resultArr -> Some (promoted_typei ::resultArr)
                                                                |_->None)
                                        |_->None)
                                      |Some (translated_argi, (BasicExpTy BoolTy)) ->
                                        (match  (applyArgs rest rest_types) with
                                        |Some resultArr -> Some (translated_argi ::resultArr)
                                        |_->None)
                                      |_->None)
                                  |(_,_)-> None )
                               in
                               (match  applyArgs explist varTypeslst with
                               |Some finalargs -> Some(CallExp (s, finalargs), exp_type_of_function_return_type f_return_type)
                               |_->None)
                            |_->None)