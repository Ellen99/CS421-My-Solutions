(* File: cmm_typed_stmt_of_untyped_stmt.ml *)
(* Author: Elsa L. Gunter *)
(* Spring 2023 *)
(* Share and Enjoy *)

open Common
open Plsolution

let update_type_env_with_varDecl (env:(string * declarable_type) list) 
                                  ((var_type:basic_type), (var_decls:varDeclId list))
                                    :(string * declarable_type) list = 
  let rec createPairs environment lst =
    match lst with
    |[]-> environment
    |(VarId s)::xs -> (createPairs (ins_env environment s (VarTy (BasicVarTy var_type))) xs)
    |(ArrayId (str,_)::xs2) -> (createPairs (ins_env environment str (VarTy (ArrayVarTy var_type))) xs2)
  in (createPairs env var_decls) 

let rec typed_statement_of_untyped_statement (ret_type:function_return_type)
                                             (env:(string * declarable_type) list)
                                             (stmt:(untyped_monop, untyped_binop) statement)
                                             :((typed_monop, typed_binop) statement * statement_return_possibility) option
= match stmt with
|AssignStatement(s,exp) -> (match lookup_env env s with
                            |Some (VarTy type_exp)-> (match typed_exp_of_untyped_exp env exp
                                             with
                                             |Some (translated_exp, retType) ->
                                                    (match (promote (translated_exp, retType) (exp_type_of_var_type type_exp) ) with
                                                    |Some typed_expr -> Some (AssignStatement (s, typed_expr), NoReturn)
                                                    |_->None )
                                             |_-> None)
                            |_-> None)

| ArrayAssignStatement (s, exp1, exp2)-> (match lookup_env env s with
                                         |Some (VarTy (ArrayVarTy celltype)) -> (match (typed_exp_of_untyped_exp env exp1, 
                                                                    typed_exp_of_untyped_exp env exp2) with
                                                            |(Some (translated_exp1, translated_exp1_type),
                                                              Some (translated_exp2, translated_exp2_type)) -> 
                                                                (match (promote (translated_exp1, translated_exp1_type) (BasicExpTy (NumTy IntTy)),
                                                                        promote (translated_exp2, translated_exp2_type) (BasicExpTy celltype))
                                                                 with
                                                                |(Some promoted_exp1,Some promoted_exp2) -> Some (ArrayAssignStatement (s, promoted_exp1, promoted_exp2), NoReturn)
                                                                |_->None)
                                                            |_->None)
                                         |_->None)

 | ExpStatement exp -> (match typed_exp_of_untyped_exp env exp with
                        |Some (translated_exp, translated_exp_type) -> Some (ExpStatement translated_exp, NoReturn)
                        |_-> None)

| PrintStatement exp -> (match typed_exp_of_untyped_exp env exp with
                        |Some (typed_exp, ArrayVarExpTy (NumTy CharTy)) -> Some (PrintStatement typed_exp,NoReturn)
                        |_->None
                        )

 |ReturnStatement exp -> (match exp with
                          |Some e -> (match typed_exp_of_untyped_exp env e with
                                      |Some (translated_exp, translated_exp_type) -> 
                                            (match promote (translated_exp, translated_exp_type) (exp_type_of_function_return_type ret_type) with
                                              |Some promoted_exp -> Some (ReturnStatement (Some promoted_exp), WillReturn)
                                              |_->None)
                                      |_->None)
                          |None ->  if ret_type = VoidFunTy then( Some (ReturnStatement None, WillReturn) ) else None)
| BlockStatement (var_decls, stmts) ->
                          (match typed_compound_statement_of_untyped_compound_statement ret_type env (var_decls, stmts) with
                          | Some (typed_stmts, possibility) -> Some (BlockStatement typed_stmts, possibility)
                          | None -> None)
|IfStatement(guard, then_stmt)-> 
  (match typed_exp_of_untyped_exp env guard with
  | Some (typed_guard, BasicExpTy BoolTy) ->
      (match typed_compound_statement_of_untyped_compound_statement ret_type env then_stmt with
      | Some (typed_then_stmt, then_ret_kind) ->
      if then_ret_kind = NoReturn then Some (IfStatement (typed_guard, typed_then_stmt), then_ret_kind)
      else  Some (IfStatement (typed_guard, typed_then_stmt), MaybeReturn)
      | None -> None)
  | _ -> None)

|WhileStatement (guard, then_stmt)  -> 
  (match typed_exp_of_untyped_exp env guard with
  | Some (typed_guard, BasicExpTy BoolTy) ->
      (match typed_compound_statement_of_untyped_compound_statement ret_type env then_stmt with
      | Some (typed_then_stmt, then_ret_kind) ->
      if then_ret_kind = NoReturn then Some (WhileStatement (typed_guard, typed_then_stmt), then_ret_kind)
      else  Some (WhileStatement (typed_guard, typed_then_stmt), MaybeReturn)
      | None -> None)
  | _ -> None)
|IfElseStatement (exp, compstmt1,compstmt2) ->  (match (typed_statement_of_untyped_statement ret_type env (IfStatement (exp, compstmt1)) ,
                                                        typed_statement_of_untyped_statement ret_type env (IfStatement (exp, compstmt2))) with
                                                |(Some (IfStatement (translated_then_exp, typed_then_stmt),possibility1),
                                                  Some (IfStatement (translated_else_exp, typed_else_stmt), possibility2)) ->
                                                  if possibility1 = possibility2 && (possibility1 = WillReturn || possibility2 = MaybeReturn) then Some (IfElseStatement (translated_else_exp, typed_then_stmt, typed_else_stmt), WillReturn)
                                                  else if possibility1 = possibility2 && possibility1 = NoReturn  then Some (IfElseStatement (translated_else_exp, typed_then_stmt, typed_else_stmt), NoReturn)
                                                  else Some (IfElseStatement (translated_else_exp, typed_then_stmt, typed_else_stmt), MaybeReturn)
                                                |_->None)

and typed_compound_statement_of_untyped_compound_statement 
                         (ret_type:function_return_type) 
                         (env: (string * declarable_type) list)
                         ((var_decls:varDecl list), (stmts:(untyped_monop, untyped_binop) statement list) )
                         :((typed_monop, typed_binop) compound_statement * statement_return_possibility) option
= let updated_env = List.fold_left (fun env' var_decl -> 
update_type_env_with_varDecl env' var_decl) env var_decls
in
let rec process_statements (stmts:(untyped_monop, untyped_binop) statement list) 
                            (u_env:(string * declarable_type) list)
                            (max_return_possibility: statement_return_possibility): ((typed_monop, typed_binop) statement list * statement_return_possibility) option =
  match stmts with
  | [] -> Some ([], max_return_possibility)
  | stmt :: rest ->
    (match typed_statement_of_untyped_statement ret_type u_env stmt with
    | Some (typed_stmt, stmt_return_kind) ->
    let max_return_possib = (match (max_return_possibility, stmt_return_kind) with
                                  |(WillReturn, WillReturn) | (WillReturn, MaybeReturn) | (WillReturn, NoReturn)
                                  |(MaybeReturn, MaybeReturn) | (MaybeReturn, NoReturn) | (NoReturn, NoReturn) ->  max_return_possibility
                                  |(NoReturn, WillReturn) | (NoReturn, MaybeReturn) |(MaybeReturn, WillReturn) -> stmt_return_kind) in
                                  
          if max_return_possibility = WillReturn then print_endline ("max_return_possibility DEBUG: WillReturn") else if max_return_possibility = NoReturn then print_endline ("max_return_possibility DEBUG: NoReturn") else if max_return_possibility = MaybeReturn then print_endline ("max_return_possibility DEBUG: MaybeReturn"); 
          if stmt_return_kind = WillReturn then print_endline ("stmt_return_kind DEBUG: WillReturn") else if stmt_return_kind = NoReturn then print_endline ("stmt_return_kind DEBUG: NoReturn") else if stmt_return_kind = MaybeReturn then print_endline ("stmt_return_kind DEBUG: MaybeReturn"); 

            (match process_statements rest u_env max_return_possib with
            | Some (typed_rest, rest_return_kind) ->if rest_return_kind = WillReturn then print_endline ("rest_return_kind DEBUG: WillReturn") else if rest_return_kind = NoReturn then print_endline ("rest_return_kind DEBUG: NoReturn") else if rest_return_kind = MaybeReturn then print_endline ("rest_return_kind DEBUG: MaybeReturn"); 
            Some (typed_stmt :: typed_rest, rest_return_kind)
            | _ -> None)
    |_->None)
in
match process_statements stmts updated_env NoReturn with
| Some (typed_stmts, return_kind) -> Some ((var_decls, typed_stmts), return_kind)
| None -> None

(* [0 / 1] typed_compound_statement_of_untyped_compound_statement (BasicFunTy (NumTy IntTy)) [] ([(NumTy IntTy, [VarId "x"; VarId "y"])], [ AssignStatement ("x", ConstExp (IntConst 5)); IfElseStatement (RelOpAppExp (EqOp, IdentExp "x", ConstExp (IntConst 5)), ([], [ReturnStatement (Some (ConstExp (IntConst 10)))]), ([], [ReturnStatement (Some (ConstExp (IntConst 20)))])); AssignStatement ("y", ConstExp (IntConst 30)) ]) (student solution returns an incorrect value) *)

 (*
let rec process_statements (stmts:(untyped_monop, untyped_binop) statement list) 
                            (env:(string * declarable_type) list)
                            (return_kind_acc: statement_return_possibility): ((typed_monop, typed_binop) statement list * statement_return_possibility) option =
  match stmts with
  | [] -> Some ([], return_kind_acc)
  | stmt :: rest ->
    (match typed_statement_of_untyped_statement ret_type updated_env stmt with
    | Some (typed_stmt, stmt_return_kind) ->
      (match return_kind_acc with
      | WillReturn -> Some ([], WillReturn)
      | _ ->
        (match stmt_return_kind with
        | WillReturn -> Some ([typed_stmt], WillReturn)
        | MaybeReturn ->
          (match process_statements rest updated_env return_kind_acc with
          | Some (typed_rest, rest_return_kind) -> Some (typed_stmt :: typed_rest, rest_return_kind)
          | _ -> None)
        | NoReturn ->
          (match process_statements rest updated_env return_kind_acc with
          | Some (typed_rest, rest_return_kind) -> Some (typed_stmt :: typed_rest, rest_return_kind)
          | _ -> None)))
    | None -> None)
in
match process_statements stmts updated_env NoReturn with
| Some (typed_stmts, return_kind) -> Some ((var_decls, typed_stmts), return_kind)
| None -> None *)