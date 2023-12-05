(* File: common.ml *)

(* expressions for PicoML *)

type const =
     BoolConst of bool        (* for true and false *)
   | IntConst of int          (* 0,1,2, ... *)
   | FloatConst of float      (* 2.1, 3.0, 5.975, ... *)
   | StringConst of string    (* "a", "hi there", ... *)
   | NilConst                 (* [ ] *)
   | UnitConst                (* ( ) *)

let string_of_const = function
   BoolConst b     -> (if b then "true" else "false")
 | IntConst i      -> string_of_int i
 | FloatConst f     -> ((string_of_float f)^(if ceil f = floor f then ("0") else ("")))
 | StringConst s   -> ("\""^ (String.escaped s)^ "\"")
 | NilConst        -> "[]"
 | UnitConst       -> "()"

type mon_op =
     IntNegOp      (* integer negation *)
   | HdOp          (* hd *)
   | TlOp          (* tl *)
   | FstOp         (* fst *)
   | SndOp         (* snd *)

let string_of_mon_op = function
     IntNegOp -> "~"
   | HdOp -> "hd"
   | TlOp -> "tl"
   | FstOp -> "fst"
   | SndOp -> "snd"

type bin_op =
     IntPlusOp        (* _ + _ *)
   | IntMinusOp       (* _ - _ *)
   | IntTimesOp       (* _ * _ *)
   | IntDivOp         (* _ / _ *)
   | FloatPlusOp      (* _ +. _ *)
   | FloatMinusOp     (* _ -. _ *)
   | FloatTimesOp     (* _ *. _ *)
   | FloatDivOp       (* _ /. _ *)
   | ConcatOp         (* _ ^ _ *)
   | ConsOp           (* _ :: _ *)
   | CommaOp          (* _ , _ *)
   | EqOp             (* _ = _ *)
   | GreaterOp        (* _ > _ *)

let string_of_bin_op = function
     IntPlusOp  -> "+"
   | IntMinusOp -> "-"
   | IntTimesOp -> "*"
   | IntDivOp -> "/"
   | FloatPlusOp -> "+."
   | FloatMinusOp -> "-."
   | FloatTimesOp -> "*."
   | FloatDivOp -> "/."
   | ConcatOp -> "^"
   | ConsOp -> "::"
   | CommaOp -> ","
   | EqOp  -> "="
   | GreaterOp -> ">"

type exp =  (* Exceptions will be added in later MPs *)
 | VarExp of string                    (* variables *)
 | ConstExp of const                   (* constants *)
 | MonOpAppExp of mon_op * exp         (* % exp1
                    where % is a builtin monadic operator *) 
 | BinOpAppExp of bin_op * exp * exp   (* exp1 % exp2                         
                    where % is a builtin binary operator *)
 | IfExp of exp * exp * exp            (* if exp1 then exp2 else exp3 *)
 | AppExp of exp * exp                 (* exp1 exp2 *) 
 | FunExp of string * exp              (* fun x -> exp1 *)
 | LetInExp of string * exp * exp      (* let x = exp1 in exp2 *)
 | LetRecInExp of string * string * exp * exp 
                                       (* let rec f x = exp1 in exp2 *)


let rec string_of_exp = function
   VarExp s -> s
 | ConstExp c ->  string_of_const c
 | IfExp(e1,e2,e3)->"if " ^ (string_of_exp e1) ^
                 " then " ^ (string_of_exp e2) ^
                 " else " ^ (string_of_exp e3)
 | MonOpAppExp (m,e) ->  (string_of_mon_op m) ^ " " ^ (paren_string_of_exp e) 
 | BinOpAppExp (b,e1,e2) -> 
   (match b
    with CommaOp ->
           ("(" ^ (paren_string_of_exp e1) ^ (string_of_bin_op b) ^
               (paren_string_of_exp e2) ^ ")")
      | _ -> ((paren_string_of_exp e1) ^ " " ^ (string_of_bin_op b)
              ^ " " ^ (paren_string_of_exp e2)))
 | AppExp(e1,e2) ->
     (non_app_paren_string_of_exp e1) ^ " " ^ (paren_string_of_exp e2) 
 | FunExp (x,e) ->  ("fun " ^ x ^ " -> " ^ (string_of_exp e))
 | LetInExp (x,e1,e2) ->
     ("let "^x^" = "^ (string_of_exp e1) ^ " in " ^ (string_of_exp e2))
 | LetRecInExp (f,x,e1,e2) ->
     ("let rec "^f^" "^x^" = "^(string_of_exp e1) ^ " in " ^ (string_of_exp e2))

and paren_string_of_exp e =
    match e with VarExp _ | ConstExp _ -> string_of_exp e
    | _ -> "(" ^ string_of_exp e ^ ")"

and non_app_paren_string_of_exp e =
    match e with AppExp (_,_) -> string_of_exp e
    | _ -> paren_string_of_exp e

let print_exp exp = print_string (string_of_exp exp)

let rec mergesort list =
let split l =
  let rec split_aux l left right = 
    match l,left,right with
    | ([] | [_]),_,_ -> (List.rev left),right
    | (_::_::t),_,h::right_t -> split_aux t (h::left) right_t
    | _ -> assert false
  in
  split_aux l [] l
  in
let rec merge l1 l2 =
  match l1,l2 with
  | [],l | l,[] -> l
  | h1::t1,h2::t2 ->
    if h1 < h2  then h1::(merge t1 l2)
    else if h2 < h1 then h2::(merge l1 t2)
    else merge t1 l2
  in match list with
  | ([] | [_]) as l -> l
  | l ->  let left,right = split l in 
          merge (mergesort left) (mergesort right)

type cont_var = Kvar                      (* _k *)

type cps_cont = 
   External
 | ContVarCPS of cont_var                 (* _k *)
 | FnContCPS of string * exp_cps          (* FN x -> exp_cps *)

and exp_cps =
   VarCPS of cps_cont * string                            (* K x *)
 | ConstCPS of cps_cont * const                           (* K c *)
 | MonOpAppCPS of cps_cont * mon_op * string              (* K (% x) *)
 | BinOpAppCPS of cps_cont * bin_op * string * string     (* K (x % y) *)
 | IfCPS of string * exp_cps * exp_cps  (* IF x THEN exp_cps1 ELSE exp_cps2 *)
 | AppCPS of cps_cont * string * string                   (* x y K *)
 | FunCPS of cps_cont * string * cont_var * exp_cps
                                        (* K (FUN x _k -> [[exp]]_k) *)
 | FixCPS of cps_cont * string * string * cont_var * exp_cps 
                                        (* K (FIX f. FUN x _k -> [[exp]]_k) *)

let string_of_cont_var Kvar = "_k"

let rec string_of_exp_cps ext_cps =
    match ext_cps with VarCPS (k,x) -> paren_string_of_cps_cont k ^ " " ^ x
    | ConstCPS (k,c) -> paren_string_of_cps_cont k ^ " " ^ string_of_const c
    | MonOpAppCPS (k,m,r) ->
       paren_string_of_cps_cont k ^ "(" ^  string_of_mon_op m ^ " " ^ r ^ ")"
    | BinOpAppCPS (k,b,r,s) ->
       paren_string_of_cps_cont k ^ "(" ^ r ^ " " ^ string_of_bin_op b ^ " " ^ s ^")"
    | IfCPS (b,e1,e2) -> "IF "^b^" THEN "^ string_of_exp_cps e1 ^" ELSE "^string_of_exp_cps e2
    | AppCPS (k,r,s) -> "("^r ^ " " ^ s ^ " " ^ paren_string_of_cps_cont k ^ ")" 
    | FunCPS (k, x, Kvar, e) ->  (paren_string_of_cps_cont k) ^ " (" ^ (string_of_funk x e) ^ ")"
    | FixCPS (k,f,x,Kvar, e) -> paren_string_of_cps_cont k ^
                            "(FIX "^ f ^". " ^ (string_of_funk x e) ^ ")"
and string_of_funk x e =
     "FUN " ^ x ^ " " ^ (string_of_cont_var Kvar) ^ " -> " ^ string_of_exp_cps e
and
   string_of_cps_cont k =
    match k with External -> "<external>"
    | ContVarCPS Kvar -> string_of_cont_var Kvar
    | FnContCPS (x, e) -> "FN " ^ x ^ " -> " ^ string_of_exp_cps e
and
  paren_string_of_cps_cont k =
   match k with FnContCPS _ -> "(" ^ string_of_cps_cont k ^ ")"
   | _ -> string_of_cps_cont k

let rec freeVarsInExpCPS cont =
    match cont with VarCPS (k, x) -> x :: freeVarsInContCPS k
    | ConstCPS (k, c) -> freeVarsInContCPS k
    | MonOpAppCPS (k,m,s) -> s :: freeVarsInContCPS k
    | BinOpAppCPS (k,b,r,s) -> r :: s :: freeVarsInContCPS k
    | IfCPS (r,e1,e2) -> r :: ((freeVarsInExpCPS e1) @ (freeVarsInExpCPS e2))
    | AppCPS (k,x1,x2) -> x1::x2::(freeVarsInContCPS k)
    | FunCPS (k,x,Kvar,e) ->
      (freeVarsInContCPS k) @ (List.filter (fun y -> not (x = y)) (freeVarsInExpCPS e))
    | FixCPS (k,f,x,Kvar,e) -> (freeVarsInContCPS k) @ 
      (List.filter (fun y -> not ((x = y) || (f = y))) (freeVarsInExpCPS e)) 
and
   freeVarsInContCPS k =
   match k with External -> []
   | ContVarCPS _ -> []
   | FnContCPS (k, e) -> (freeVarsInExpCPS e)


(* Fresh Name stuff *)

let int_to_string n =
    let int_to_int_26_list n =
        let rec aux n l =
            if n <= 0 then l else let c = ((n-1) mod 26) in aux ((n -(c+1))/26) (c::l)
        in aux n []
    in
        let rec aux l = match l with [] -> ""
                            | n::ns -> (String.make 1 (Char.chr (n + 97))) ^ aux ns
        in aux (int_to_int_26_list n)

let freshFor lst = 
    let rec fresh_ n = 
        if List.mem (int_to_string n) lst
           then fresh_ (n+1)
        else int_to_string n
    in fresh_ 1

(* End Fresh name stuff *)

(* Normalization functions for equality testing *)

let rec exp_cps_norm_aux var_subst free_vars exp_cps =
 match exp_cps with
   VarCPS (kappa, var_name) ->
    let kappa' = cps_cont_norm_aux var_subst free_vars kappa
    in VarCPS (kappa',var_subst var_name) 
 | ConstCPS (kappa, const) ->
   let kappa' = cps_cont_norm_aux var_subst free_vars kappa
    in ConstCPS (kappa', const)
 | MonOpAppCPS (kappa, binop, argvar) ->
   let kappa' = cps_cont_norm_aux var_subst free_vars kappa
    in MonOpAppCPS (kappa', binop, var_subst argvar)
 | BinOpAppCPS (kappa, binop, fstvar, sndvar) ->
   let kappa' = cps_cont_norm_aux var_subst free_vars kappa
    in BinOpAppCPS (kappa', binop, var_subst fstvar, var_subst sndvar)
 | IfCPS (boolvar, thenexpcps, elseexpcps) ->
   let thenexpcps' = exp_cps_norm_aux var_subst free_vars thenexpcps
   in
   let elseexpcps' = exp_cps_norm_aux var_subst free_vars elseexpcps
   in IfCPS (var_subst boolvar, thenexpcps', elseexpcps')
 | AppCPS (kappa, funvar, argvar) ->
   let kappa' = cps_cont_norm_aux var_subst free_vars kappa
   in AppCPS (kappa', var_subst funvar, var_subst argvar)
 | FunCPS (kappa, x, k, bodyexpcps) -> 
   let kappa' = cps_cont_norm_aux var_subst free_vars kappa
   in
   (*we will keep x; it is "user supplied"*)
   let new_var_subst name = if name = x then name else var_subst name 
   in
   let bodyexpcps' = exp_cps_norm_aux new_var_subst (x::free_vars) bodyexpcps
   in FunCPS (kappa', x, k, bodyexpcps')
 | FixCPS (kappa, f, x, k, bodyexpcps) ->
   let kappa' = cps_cont_norm_aux var_subst free_vars kappa
   in
   (*we will keep f and x; they are "user supplied"*)
   let new_var_subst name =
       if name = x || name = f then name else var_subst name 
   in
   let bodyexpcps' = exp_cps_norm_aux new_var_subst (f::x::free_vars) bodyexpcps
   in FixCPS (kappa', f, x, k, bodyexpcps')

and cps_cont_norm_aux var_subst free_vars kappa =
 match kappa with
   External -> External
 | ContVarCPS k -> ContVarCPS k
 | FnContCPS (y, bodyexpcps) ->
   let newy = freshFor free_vars
   in
   let new_var_subst name =
       if name = y then newy else var_subst name 
   in
   let bodyexpcps' = exp_cps_norm_aux new_var_subst (newy::free_vars) bodyexpcps
   in FnContCPS (newy, bodyexpcps')

let exp_cps_normalize ec fv = exp_cps_norm_aux (fun s -> s) fv ec
