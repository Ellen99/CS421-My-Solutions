(* File: common.ml *)

let addk (a, b) k = k (a + b);;
let subk (a, b) k = k (a - b);;
let mulk (a, b) k = k (a * b);;
let modk (a, b) k = k (a mod b);;
let float_addk (a, b) k = k (a +. b);;
let float_subk (a, b) k = k (a -. b);;
let float_mulk (a, b) k = k (a *. b);;
let geqk (a, b) k = k (a >= b);;
let leqk (a, b) k = k (a <= b);;
let gtk (a, b) k = k (a > b);;
let ltk (a, b) k = k (a < b);;
let eqk (a, b) k = k (a = b);;
let neqk (a, b) k = k (a <> b);;
let notk b k = k (not b);;
