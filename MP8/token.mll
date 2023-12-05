{
open Common;;

}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)

let numeric = ['0' - '9']
let lowercase = ['a' - 'z']
let uppercase = ['A' - 'Z']
let letter =['a' - 'z' 'A' - 'Z' '_']
let open_comment = "(*"
let close_comment = "*)"

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }  (* skip over whitespace *)
  | eof             { EOF }
(* your rules go here *)
  | numeric+ as i {INT (int_of_string i)}
  | "0b"('0' | '1')+ as b {INT (int_of_string b)} (* Binary numbers *)
  | "0x"(numeric | ['a' - 'f'])+ as h {INT (int_of_string h)} (* Hex *)
  | numeric+ '.' numeric* as f { FLOAT (float_of_string f)}  (*  Floats *)    
  | numeric+ '.' numeric* 'e' numeric+ as s_f { FLOAT (float_of_string s_f)} (* Scientific Floats *)
  | "~" {NEG }
  | "+"	{PLUS}
  | "->" {ARROW}
  | "-" {MINUS}
  | "*" {TIMES}
  | "/" {DIV}
  | "+." {DPLUS}
  | "-." {DMINUS}
  | "*." {DTIMES}
  | "/." {DDIV}
  | "^" {CARAT}
  | "<>" {NEQ}
  | "<=" {LEQ}
  | ">=" {GEQ}
  | "<" {LT}
  | ">" {GT}
  | "=" {EQUALS}
  | "|" {PIPE}
  | ";;" {DSEMI}
  | ";" {SEMI}
  | "::" {DCOLON}
  | "@" {AT}
  | "[" {LBRAC}
  | "]" {RBRAC}
  | "[]" {NIL}
  | "let" {LET}
  | "rec" {REC}
  | "and" {AND}
  | "end" {END}
  | "in" {IN}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | "fun" {FUN}
  | "mod" {MOD}
  | "raise" {RAISE}
  | "try" {TRY}
  | "with" {WITH}
  | "not" {NOT}
  | "&&" {LOGICALAND}
  | "||" {LOGICALOR}
  | "()" {UNIT}
  | "(" {LPAREN}
  | ")" {RPAREN}
  | "," {COMMA}
  | "_" {UNDERSCORE}
  | "true" {TRUE}
  | "false" {FALSE} 
  | lowercase(letter | "'" | numeric)* as i {IDENT i} (*Identifiers*)
  | open_comment {comment 1 lexbuf}
  | ("//"([^'\n']*)) {token lexbuf}
  | close_comment {raise (Failure "unmatched closed comment")}
  | "\"" {string "" lexbuf}
and comment i = parse
  | open_comment {comment (i+1) lexbuf}
  | close_comment {if i =1 then (token lexbuf) else (comment (i-1) lexbuf)}
  |





  and comment depth = parse 
      | open_comment {comment (depth +1) lexbuf }
      | close_comment {if depth = 1 then (token lexbuf) else (comment (depth -1) lexbuf) }
      | eof {raise (Failure "unmatched open comment")}
      |_ {comment depth lexbuf }

  and string str = parse
   | "\"" {STRING str}
   | ['\t' '\n' '\r' '\b'] {raise (Failure "character is not allowed in string")}
   | "\\" {escapeStrings str lexbuf}
   | _ as s {string (str ^ (String.make 1 s))  lexbuf}

  and escapeStrings str = parse
   | "\""   { string (str ^ "\"") lexbuf }
   | "t"    { string (str ^ "\t") lexbuf }
   | "n"    { string (str ^ "\n") lexbuf }
   | "r"    { string (str ^ "\r") lexbuf }
   | "b"    { string (str ^ "\b") lexbuf }
   | " "	   { string (str ^ " ") lexbuf }
   | ['0'-'1']['0'-'9']['0'-'9'] as c {string (str ^ (String.make 1 (char_of_int (int_of_string c)))) lexbuf}
   | ['2']['0'-'4']['0'-'9']  as c {string (str ^ (String.make 1 (char_of_int (int_of_string c)))) lexbuf}
   | ['2']['5']['0'-'5'] as c {string (str ^ (String.make 1 (char_of_int (int_of_string c)))) lexbuf}
   |"\n" { skipWhiteSpace str lexbuf}
   
   and skipWhiteSpace str = parse
   |' ' {skipWhiteSpace str lexbuf}
   |"\n" {skipWhiteSpace str lexbuf}
   |"\t" {skipWhiteSpace str lexbuf}
   |"\"" {STRING str}
   |"\\" {escapeStrings str lexbuf}
   | _ as s {string (str ^ (String.make 1 s))  lexbuf}

{(* do not modify this function: *)
 let lextest s = token (Lexing.from_string s)

 let get_all_tokens s =
     let b = Lexing.from_string (s^"\n") in
     let rec g () = 
     match token b with EOF -> []
     | t -> t :: g () in
     g ()

let try_get_all_tokens s =
    try (Some (get_all_tokens s), true)
    with Failure "unmatched open comment" -> (None, true)
       | Failure "unmatched closed comment" -> (None, false)
 }

