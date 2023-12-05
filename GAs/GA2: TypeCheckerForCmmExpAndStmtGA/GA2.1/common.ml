(* File: common.ml *)

(* From cmm_ast.ml *)

(*-------------------- Cmm Type Type Information ---------------------*)

(* External and internal type information *)
type numeric_type =
    CharTy
  | IntTy
  | FloatTy
    
type basic_type =
    BoolTy
  | NumTy of numeric_type

type var_type =
    BasicVarTy of basic_type
  | ArrayVarTy of basic_type

type function_return_type =
    BasicFunTy of basic_type
  | VoidFunTy

type declarable_type =
    VarTy of var_type
  | FunTy of function_return_type * var_type list

type exp_type =
    BasicExpTy of basic_type
  | ArrayVarExpTy of basic_type
  | VoidExpTy
      
let exp_type_of_var_type vty =
  match vty
  with BasicVarTy basic_type -> BasicExpTy basic_type
    | ArrayVarTy basic_type -> ArrayVarExpTy basic_type

let exp_type_of_function_return_type frty =
  match frty
  with BasicFunTy basic_type -> BasicExpTy basic_type
    | VoidFunTy -> VoidExpTy

(*--------------- Constant and Operator Information -------------------*)

type const =
    BoolConst of bool
  | CharConst of char
  | IntConst of int
  | FloatConst of float

type untyped_monop =  NegOp | NotOp

type typed_monop =  IntNegOp | FloatNegOp | BoolNotOp |IntOfChar | FloatOfInt

type untyped_binop = PlusOp | MinusOp | TimesOp | DivOp | ModOp | ConcatOp

type typed_binop =
    IntPlusOp
  | FloatPlusOp
  | IntMinusOp
  | FloatMinusOp
  | IntTimesOp
  | FloatTimesOp
  | IntDivOp
  | FloatDivOp           
  | IntModOp
  | StringConcatOp


type rel_op = EqOp | NotEqOp | LessOp | LessEqOp | GreaterOp | GreaterEqOp

(* -----------------------Expressions for Cmm ---------------------*)

type ('monop, 'binop) exp =
     ConstExp of const
  | IdentExp of string
  | ArrayEltExp of string * ('monop, 'binop) exp
  | MonOpAppExp of 'monop * ('monop, 'binop) exp
  | BinOpAppExp of 'binop * ('monop, 'binop) exp * ('monop, 'binop) exp
  | RelOpAppExp of rel_op * ('monop, 'binop) exp * ('monop, 'binop) exp
  | AndExp of ('monop, 'binop) exp * ('monop, 'binop) exp
  | OrExp of ('monop, 'binop) exp * ('monop, 'binop) exp
  | CallExp of string * ('monop, 'binop) exp list

type untyped_exp = (untyped_monop, untyped_binop) exp
type typed_exp = (typed_monop, typed_binop) exp

(*-------------- Variable and Array Declarations for Cmm -------------*)

type varDeclId =
    VarId of string
  | ArrayId of string * int

type varDecl = basic_type * varDeclId list

(*---------------------- Statements for Cmm ----------------------*)

type ('monop, 'binop) statement = 
    AssignStatement of string * ('monop, 'binop) exp
  | ArrayAssignStatement
      of string * ('monop, 'binop) exp * ('monop, 'binop) exp
  | ExpStatement of ('monop, 'binop) exp
  | PrintStatement of ('monop, 'binop) exp
  | BlockStatement of ('monop, 'binop) compound_statement
  | IfStatement
      of ('monop, 'binop) exp * ('monop, 'binop) compound_statement
  | IfElseStatement
      of ('monop, 'binop) exp *
          ('monop, 'binop) compound_statement *
          ('monop, 'binop) compound_statement
  | WhileStatement
      of ('monop, 'binop) exp * ('monop, 'binop) compound_statement
  | ReturnStatement of ('monop, 'binop) exp option

and ('monop, 'binop) compound_statement =
    varDecl list * ('monop, 'binop) statement list

type untyped_statement = (untyped_monop, untyped_binop) statement
type untyped_compound_statement =
    (untyped_monop, untyped_binop) compound_statement

type statement_return_possibility =
    NoReturn | MaybeReturn | WillReturn

(*------------ Function Declarations and Programs for Cmm --------------*)

type paramID = ((*var_name*) string * (*is_array*) bool)

type paramType = basic_type * paramID

type params = paramType list

type ('monop, 'binop) decl =
    VarDecl of varDecl
  | FunDecl of
      function_return_type * string * params *
        ('monop, 'binop) compound_statement

type untyped_decl = (untyped_monop, untyped_binop) decl

type ('monop, 'binop) program =
    ('monop, 'binop) decl list * ('monop, 'binop) exp

type untyped_program = (untyped_monop, untyped_binop)program


(*-------------- Association lists, maps, environments ---------------*)
(* From gen_env.ml *)
(* environments *)

type ('a,'b) map = ('a * 'b) list
type 'a env = (string, 'a) map

(*environment operations*)
(*
let rec lookup mapping x =
  match mapping with
     []        -> None
   | (y,z)::ys -> if x = y then Some z else lookup ys x
*)
let rec lookup_map compare_key mapping key =
  match mapping
  with [] -> None
    | (key1,v1) :: more_map ->
      (let c = compare_key key key1 in
       if c < 0 then None
       else if c = 0 then Some v1
       else lookup_map compare_key more_map key)

let make_map key value = [(key,value)]

let rec update_map compare_key delta gamma =
  match (delta, gamma)
  with (_,[]) -> delta
    | ([],_) -> gamma
    | (((key1,val1)as delt)::rem_delta, ((key2,val2) as gelt) ::rem_gamma) ->
      let c = compare_key key1 key2 in
      if c < 0 then delt :: update_map compare_key rem_delta gamma
      else if c = 0 then delt :: update_map compare_key rem_delta rem_gamma
      else gelt :: update_map compare_key delta rem_gamma

let ins_map compare_key map key value =
  update_map compare_key [(key,value)] map 

let canonicalize_map compare_key canonicalize_value map =
  List.fold_right
    (fun (key,value) -> fun canon_map ->
      ins_map compare_key canon_map key (canonicalize_value value))
    map
    []

let make_env = (make_map: string -> 'a -> 'a env)
let lookup_env env =
  ((lookup_map String.compare env): string -> 'a option)
let ins_env (gamma) key value = ins_map String.compare gamma key value
let sum_env (delta) (gamma) = update_map String.compare delta gamma
let canonicalize_env canonicalize_value (env:'a env) =
  ((canonicalize_map String.compare canonicalize_value env):'a env)

