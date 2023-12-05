(* File: ml3.ml *)

open Common

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
