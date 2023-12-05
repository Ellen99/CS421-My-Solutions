(* File: ml3.ml *)

open Common
(*| LetInExp of string * exp * exp      (* let x = exp1 in exp2 *)
| LetRecInExp of string * string * exp * exp (* let rec f x = exp1 in exp2 *)
| LetRecInExp of f * x * exp1 * exp2 (* let rec f x = exp1 in exp2 *)*)

  
let pair_sums : exp =
  LetRecInExp ("pair_sums",
               "lst",
               IfExp(BinOpAppExp(EqOp, VarExp("lst"), ConstExp(NilConst)),
                     ConstExp(NilConst),
                     (LetInExp("x",
                               MonOpAppExp(HdOp, VarExp("lst")), 
                               BinOpAppExp(ConsOp,
                                           (BinOpAppExp(IntPlusOp,
                                                        MonOpAppExp(FstOp, VarExp("x")),
                                                        MonOpAppExp(SndOp, VarExp("x")))),
                                           (AppExp(VarExp ("pair_sums"), (MonOpAppExp(TlOp, VarExp("lst"))))))))
                      ),
               AppExp(VarExp ("pair_sums"),
                      BinOpAppExp(ConsOp, 
                                  BinOpAppExp(CommaOp, ConstExp (IntConst 7), ConstExp (IntConst 1)),
                                  BinOpAppExp(ConsOp, 
                                              BinOpAppExp(CommaOp, ConstExp (IntConst 4), ConstExp (IntConst 2)),
                                              BinOpAppExp(ConsOp,
                                                          BinOpAppExp(CommaOp, ConstExp (IntConst 6), ConstExp (IntConst 3)),
                                                          ConstExp(NilConst))))))
(*                                               
let rec pair_sums lst =
  if lst = [] then []
  else let x = (hd lst) in (fst x + snd x) :: (pair_sums (tl lst))
in pair_sums [(7,1);(4,2);(6,3)] *)

      (*
  FunExp("lst",
    If(
      Eq(Var("lst"), List([])),
      List([]),
      Let("x", Hd(Var("lst")),
        Cons(Add(Fst(Var("x")), Snd(Var("x"))), App(Var("pair_sums"), Tl(Var("lst"))))
      )
    )
  )
  *)
