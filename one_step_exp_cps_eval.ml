(* File: one_step_exp_cps_eval.ml *)

open Common;;
(* Implement app_cont_to_value. If the continuation is of the form External, you should return Final of the value.
 If it is a continuation variable, look it up in the environment and apply the resulting continuation.
 
 If the continuation is of the form , then create the one-step intermediate result
  given by adding to the memory the binding of  to the value, 
  and bundling this with the CPS expression . 
  
  If the continuation is an exception handler continuation (ExnMatch), and the value is an integer, then apply the exception handler continuation to the integer to yield the result.

To apply an exception handler continuation (exn_cont) that is an exception handler continuation variable, look the variable up in the environment and apply the resulting exception handler continuation to the integer. If the handler continuation is empty, the result is an UncaughtException. If the handler continuation is an update of an earlier handler continuation, , by a partial mapping of integer options to exception continuations, and either the given integer or None is assigned an exception handler continuation by the partial mapping, then return the one-step intermediate result using the given memory and the first expression continuation just found. If no match exists in the partial mapping, then recursively apply . *)

let rec app_cont_exn_handler_to_integer (env:cps_env_entry list) (ex:exn_cont) (i:int) :step_result  = 
  match ex with
  | ExnContVarCPS expi -> let handler =  lookup_exn_cont env expi in
                          (match handler with
                          |Some (contexn, memory) -> app_cont_exn_handler_to_integer memory contexn i
                          |None->Failed
                          )
  | EmptyExnContCPS -> UncaughtException(i)
  | UpdateExnContCPS (partialMapping, epsilon_cont) -> let rec findmapping_for_i lst = 
                                                        (match lst with
                                                          |(None, cont_)::ls -> Intermediate (env,cont_)
                                                          |(Some id, cont_)::ls -> if id = i then Intermediate (env,cont_)
                                                                                  else findmapping_for_i ls
                                                          |[]-> app_cont_exn_handler_to_integer env epsilon_cont i
                                                          )
                                                        in findmapping_for_i partialMapping
      


let rec app_cont_to_value (env:cps_env_entry list) (k:cps_cont) (v:value) :step_result = 
  match k with
  | External -> Final v
  | ContVarCPS cvar -> let found = lookup_cont env cvar in
                        (match found with
                         | None -> Failed
                         | Some (c, found_entry_lst) -> app_cont_to_value found_entry_lst c v
                        ) 
  | FnContCPS (y, e) -> Intermediate ((ValueBinding (y,v))::env, e)
  | ExnMatch e -> match v with 
                  |IntVal i -> app_cont_exn_handler_to_integer env e i
                  |_->Failed

  
let rec one_step_exp_cps_eval (env:cps_env_entry list) (exp_cps:exp_cps):step_result = 
  match exp_cps with
  | ConstCPS (cps_cont, c) -> app_cont_to_value env cps_cont (const_to_val c)
  | VarCPS (cps_cont, s) -> (match lookup_value env s with
                            |None -> Failed
                            |Some found -> app_cont_to_value env cps_cont found
                            )
  | MonOpAppCPS (cps_cont, mon_op, s, exn_cont) -> 
        (match lookup_value env s with
         |None -> Failed
         |Some v -> (match monOpApply mon_op v with
                    |Value res_v -> app_cont_to_value env cps_cont res_v
                    |Exn exn_i -> app_cont_exn_handler_to_integer env exn_cont exn_i))

  |BinOpAppCPS (cps_cont, bin_op , s1, s2, exn_cont) -> 
        (match (lookup_value env s1,lookup_value env s2) with
          |(Some v1, Some v2) -> (match binOpApply bin_op v1 v2 with
                                  |Value res_v -> app_cont_to_value env cps_cont res_v 
                                  |Exn exn_i -> app_cont_exn_handler_to_integer env exn_cont exn_i
                                  )
          |_-> Failed)
  | IfCPS (s, exp_cps1, exp_cps2) -> 
        (match lookup_value env s with
          |None -> Failed
          |Some v  ->if v = (BoolVal true) then Intermediate(env,exp_cps1)
                      else  Intermediate(env,exp_cps2)
          )
  | FunCPS (cps_cont, s, cont_var, i, exp_cps) -> 
      let closure = CPSClosureVal (s, cont_var, i, exp_cps, env) in
      app_cont_to_value env cps_cont closure
  | FixCPS (cps_cont, s1, s2, cont_var, i, exp_cps) ->
      let rec_closure = CPSRecClosureVal (s1,s2,cont_var, i, exp_cps, env) in
      app_cont_to_value env cps_cont rec_closure

  |AppCPS (cps_cont, f, x, exn_cont) ->
    (match (lookup_value env x, lookup_value env f) with
      |(Some v, Some (CPSClosureVal  (y, k, ek, e, env')) ) ->  
        Intermediate ([ValueBinding (y,v);ContBinding (k, (cps_cont, env));ExnContBinding (ek, (exn_cont, env))] @ env', e)
      |(Some v, Some (CPSRecClosureVal  (g,y,k, ek, e, env')))  -> 
        Intermediate ([ValueBinding (y,v);ValueBinding (g,CPSRecClosureVal  (g,y,k, ek, e, env'))]
                     @[ContBinding (k, (cps_cont, env));ExnContBinding (ek, (exn_cont, env))] @ env', e)
      |_-> Failed
     )
