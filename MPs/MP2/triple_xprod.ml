(* File: triple_xprod.ml *)

let triple_xprod trp pr = match (trp, pr) with | ((t1,t2,t3), (p1,p2))->(((t1,p1),(t2,p1),(t3,p1)),((t1,p2),(t2,p2),(t3,p2)));;
