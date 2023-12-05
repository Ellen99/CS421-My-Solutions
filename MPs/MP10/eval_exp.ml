open Common;;

let const_to_val c : value =
  match c with
  | BoolConst b -> BoolVal b
  | IntConst i-> IntVal i
  | FloatConst f -> FloatVal f
  | StringConst s-> StringVal s
  | NilConst -> ListVal []
  | UnitConst -> UnitVal
  
  
let monOpApply (op:mon_op) (v:value) : value = 
  match v with
  |IntVal i -> (match op with 
                |IntNegOp -> IntVal ( i * (-1))
                |_ -> raise (Failure "unmatched type"))
  |StringVal str -> (match op with 
                     |PrintOp -> (print_string str) ; UnitVal
                     |_->raise (Failure "unmatched type"))
  |ListVal lst -> (match lst with
                    | []-> Exn(0)
                    |(x::xs) -> (match op with
                                  |HdOp -> x
                                  |TlOp -> ListVal xs
                                  |_-> raise (Failure "unmatched type")))
                    (* monOpApply FstOp (PairVal (IntVal 2, IntVal 3));; *)
  |PairVal (v1,v2) -> (match op with
                       |FstOp -> v1
                       |SndOp -> v2
                       |_-> raise (Failure "unmatched type"))
  |_ -> raise (Failure "unmatched type")

let binOpApply binop (v1,v2) =
  (match binop with
      |CommaOp -> PairVal(v1,v2)
      |EqOp -> if v1 = v2 then BoolVal true else BoolVal false
      |GreaterOp ->  if v1 > v2 then BoolVal true else BoolVal false
      |_-> (match (v1,v2) with
           |(IntVal i1, IntVal i2) ->
              (match binop with
                |IntPlusOp -> IntVal (i1+i2)
                |IntMinusOp -> IntVal (i1-i2)
                |IntTimesOp -> IntVal (i1*i2)
                |IntDivOp -> if i2 = 0 then Exn 0
                              else  IntVal (i1/i2)
                |ModOp -> IntVal (i1 mod i2)
                |_-> raise (Failure "unmatched type"))
          |(FloatVal f1, FloatVal f2) ->
              (match binop with
                |FloatPlusOp -> FloatVal (f1+.f2)
                |FloatMinusOp -> FloatVal (f1-.f2)
                |FloatTimesOp -> FloatVal (f1*.f2)
                |FloatDivOp ->if f2 = 0.0 then Exn 0
                                else  FloatVal (f1/.f2)
                |ExpoOp -> FloatVal (f1 ** f2)
                |_-> raise (Failure "unmatched type"))
          |(StringVal s1, StringVal s2) ->
              (match binop with
                |ConcatOp -> StringVal (s1 ^ s2)
                |_-> raise (Failure "unmatched type"))
          |(hd, ListVal tl) ->
              (match binop with
                |ConsOp -> ListVal (hd::tl)
                |_-> raise (Failure "unmatched type"))  
  
   |_->  raise (Failure "unmatched type") ))

let rec eval_exp (e, m:exp * memory) : value = 
  match e with
  | VarExp s ->                     (* variables *)
    (match lookup_env m s with  
      | None -> raise (Failure "not found in memory")
      | Some v -> (match v with
                   |RecVarVal(g,y,e',m') -> let env = ins_env m' g (RecVarVal(g,y,e',m')) in
                                              Closure (y,e',env) 
                   |_ -> v)
    )
  | ConstExp c -> const_to_val c    (* constants *)
  | MonOpAppExp (mop,em) -> let v = eval_exp (em, m) in
                            (match v with
                              |Exn i -> Exn i
                              |_ -> monOpApply mop v ) (* % e1 for % is a builtin monadic operator *)
  | BinOpAppExp (bop, e1, e2)-> let val_e1 = eval_exp (e1, m) in
                                let val_e2 = eval_exp (e2 ,m) in
                                (match (val_e1, val_e2) with
                                 | (_, Exn a) -> Exn a
                                 | (Exn b, _) -> Exn b
                                 | _ -> binOpApply bop  (val_e1, val_e2))
  | IfExp (e1,e2,e3) -> (match eval_exp (e1,m) with
                          | BoolVal true -> eval_exp (e2,m)
                          | BoolVal false -> eval_exp (e3,m)
                          | Exn i -> Exn i
                          |_ -> raise (Failure "unmatched type")
                          ) 
  | LetInExp (x, e1, e2) -> let v1 = eval_exp (e1,m) in 
                            (match v1 with
                            |Exn i -> Exn i
                            |_-> let env = ins_env m x v1 in
                                  eval_exp (e2, env))
  | FunExp (x, e) -> Closure (x,e,m) 
  | AppExp (e1, e2) -> (match eval_exp (e1,m) with
                        |Closure (x, e', m') -> let v' = eval_exp (e2,m) in
                                                (match v' with
                                                |Exn j -> Exn j
                                                |_-> let env = ins_env m' x v' in
                                                     let v = eval_exp (e', env) in
                                                      (match v with
                                                      |Exn i -> Exn i
                                                      |_->v))
                                                
                        |Exn i -> Exn i
                        |_->raise (Failure "unmatched type"))
  | LetRecInExp (f,x,e1,e2) -> let env = ins_env m f (RecVarVal(f,x,e1,m)) in
                              eval_exp (e2, env)
  |RaiseExp e -> let v = eval_exp (e, m) in
                 (match v with
                  | IntVal n -> Exn n
                  | Exn i -> Exn i
                  |_ -> v)
  |TryWithExp (e1, i, e2, lst) -> let v1 = eval_exp (e1, m) in
                                  (match v1 with
                                  |Exn j ->let rec find k l= 
                                            (match l with
                                            |[]-> Exn j
                                            |(None, ei) :: xs -> (eval_exp (ei, m))
                                            |((Some ni, ei)::xs) -> if ni = k then (eval_exp (ei, m))
                                                                    else find k xs
                                            )in find j ((i, e2)::lst )
                                  |_ -> v1)
let eval_dec (dec, m) = 
    match dec with
    | Anon e -> let e_value = eval_exp (e,m) in
                  ((None, e_value), m)
    | Let (x, e) -> let e_value = eval_exp (e,m) in
                    let mem = ins_env m x e_value in
                    (match e_value with
                    |Exn i -> ((None, e_value), m)
                    |_ ->((Some x, e_value), mem)
                    )
                    
    | LetRec (f,x,e) ->((Some f, RecVarVal(f,x,e,m)), ins_env m f (RecVarVal(f,x,e,m)))