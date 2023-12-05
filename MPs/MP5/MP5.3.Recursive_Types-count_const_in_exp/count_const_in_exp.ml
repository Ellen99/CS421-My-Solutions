(* CS421 - Fall 2017
 * var
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

open Common



let rec count_const_in_exp exp = match exp with
 | VarExp s->0                    (* variables *)
 | ConstExp c->1                 (* constants *)
 | MonOpAppExp (mon_op, e) -> count_const_in_exp e       (* % exp1 where % is a builtin monadic operator *) 
 | BinOpAppExp (b,e1,e2)-> count_const_in_exp e1 + count_const_in_exp e2  (* exp1 % exp2 where % is a builtin binary operator *)
 | IfExp (e1,e2,e3) ->  count_const_in_exp e1 + count_const_in_exp e2 + count_const_in_exp e3     (* if exp1 then exp2 else exp3 *)
 | AppExp (e1,e2)-> count_const_in_exp e1 + count_const_in_exp e2               (* exp1 exp2 *) 
 | FunExp (s,e) -> count_const_in_exp e                (* fun x -> exp1 *)
 | LetInExp (s,e1,e2)-> count_const_in_exp e1 + count_const_in_exp e2      (* let x = exp1 in exp2 *)
 | LetRecInExp (s1,s2,e1,e2)-> count_const_in_exp e1 + count_const_in_exp e2

 

(* 
In class





let rec count_const_in_exp exp = match exp with
| VarExp s -> 0                    (* variables *)
| ConstExp const ->1                  (* constants *)
| MonOpAppExp (m, e)->  count_const_in_exp e       (* % exp1 where % is a builtin monadic operator *) 
| BinOpAppExp(b , e1 , e2)-> (count_const_in_exp e1) + (count_const_in_exp e2)   (* exp1 % exp2 where % is a builtin binary operator *)
| IfExp (e1,e2,e3)-> (count_const_in_exp e1) + (count_const_in_exp e2) + (count_const_in_exp e3)          (* if exp1 then exp2 else exp3 *)
| AppExp (e1,e2) -> (count_const_in_exp e1) + (count_const_in_exp e2)                 (* exp1 exp2 *) 
| FunExp (s, e1) -> (count_const_in_exp e1)          (* fun x -> exp1 *)
| LetInExp (s,e1,e2) -> (count_const_in_exp e1) + (count_const_in_exp e2)      (* let x = exp1 in exp2 *)
| LetRecInExp (s1,s2,e1,e2) -> (count_const_in_exp e1) + (count_const_in_exp e2) *)

(* 
USING FOLD
let rec foldExp (varFun: string->'a)
(constFun: const -> 'a)
(monopFun: mon_op ->'a ->'a) 
(binopFun: bin_op ->'a -> 'a -> 'a)
(funFun: string -> 'a -> 'a)
(ifFun: 'a -> 'a -> 'a -> 'a) 
(appFun: 'a -> 'a -> 'a)
(letInFun: string -> 'a -> 'a-> 'a)
(letRecInFun: string -> string -> 'a -> 'a-> 'a) 
(exp: exp) : 'a =
match exp with
|VarExp v -> varFun v
|ConstExp c -> constFun c
|MonOpAppExp (mop, e) -> monopFun mop (foldExp varFun constFun monopFun binopFun funFun ifFun appFun letInFun letRecInFun e)
| BinOpAppExp (bop, e1, e2) -> binopFun bop (foldExp varFun constFun monopFun binopFun funFun ifFun appFun letInFun letRecInFun e1) (foldExp varFun constFun monopFun binopFun funFun ifFun appFun letInFun letRecInFun e2)
| IfExp (e1, e2, e3) -> ifFun (foldExp varFun constFun monopFun binopFun funFun ifFun appFun letInFun letRecInFun  e1) (foldExp varFun constFun monopFun binopFun funFun ifFun appFun letInFun letRecInFun  e2) (foldExp varFun constFun monopFun binopFun funFun ifFun appFun letInFun letRecInFun  e3)
| AppExp (e1, e2) -> appFun (foldExp varFun constFun monopFun binopFun funFun ifFun appFun letInFun letRecInFun  e1) (foldExp varFun constFun monopFun binopFun funFun ifFun appFun letInFun letRecInFun  e2)
| FunExp (s, e) -> funFun s (foldExp varFun constFun monopFun binopFun funFun ifFun appFun letInFun letRecInFun  e) 
(* fun x -> e1 *)
| LetInExp (x, e1, e2) -> letInFun x (foldExp varFun constFun monopFun binopFun funFun ifFun appFun letInFun letRecInFun  e1) (foldExp varFun constFun monopFun binopFun funFun ifFun appFun letInFun letRecInFun  e2)
| LetRecInExp (f, x, e1, e2) -> letRecInFun f x (foldExp varFun constFun monopFun binopFun funFun ifFun appFun letInFun letRecInFun  e1) (foldExp varFun constFun monopFun binopFun funFun ifFun appFun letInFun letRecInFun  e2)

let count_const_in_exp e = foldExp (fun s-> 0)
               (fun c-> 1)
               (fun m a -> a)
               (fun b a1 a2 -> a1 + a2)
               (fun f a -> a)
               (fun a1 a2 a3 -> a1+ a2 + a3) (*if*)
               (fun a1 a2 -> a1+ a2) (*app*)
               (fun x a1 a2 -> a1+ a2) (*letin*)
               (fun x y a1 a2 -> a1+ a2) (*letin*)
               e
 *)
