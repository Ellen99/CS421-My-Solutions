(* CS421 - Fall 2017
 * var
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

open Common

let rec import_list lst = match lst with
|[]-> ConstExp NilConst
|(x1,x2)::xs -> BinOpAppExp(ConsOp, 
                BinOpAppExp(CommaOp, ConstExp (IntConst x1), ConstExp (IntConst x2)),
                  import_list xs)