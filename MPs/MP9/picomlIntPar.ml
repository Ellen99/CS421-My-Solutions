(*
  interactive-parser.ml - DO NOT EDIT
*)

open Common
open Student

(* Try to detect if something is getting piped in *)
let is_interactive = 0 = (Sys.command "[ -t 0 ]")

let _ =
  (if is_interactive
      then print_endline "\nWelcome to the Student parser \n"
      else ());
  let rec loop gamma = 
  try
    let lexbuf = Lexing.from_channel stdin
    in (if is_interactive 
          then (print_string "> "; flush stdout)
          else ());
       (try
          let dec = main (fun lb -> match Picomllex.token lb with 
                                    | EOF -> raise Picomllex.EndInput
				    | r -> r)
                    lexbuf 
          in match infer_dec gather_dec_ty_substitution gamma dec with
             | None          -> (print_string "\ndoes not type check\n";
                                 loop gamma)
             | Some pf ->
	       (match canon (Some(fresh(),pf))
                with None -> raise (Failure "This shouldn't be possible\n")
                | Some (_,p) ->
                  (match p with Proof(hyps, DecJudgment (gamma', dec, delta))
                 -> let delta_gamma = sum_env delta gamma' in
		  (let _ = List.map
		   (fun (x,pty) -> 
		    (print_string ("val "^x^" : ");
		     print_string ((string_of_monoTy (snd pty))^"\n")))
                   delta in ();
                   print_string "\n\nfinal environment:\n\n";
                   print_string (string_of_type_env delta_gamma);  
                   print_string "\n\nproof:\n";
                   print_string (string_of_proof p); 
                   loop delta_gamma)
                 | _ ->  raise (Failure "This shouldn't be possible\n")))
        with Failure s -> (print_newline();
			   print_endline s;
                           print_newline();
                           loop gamma)
           | Parsing.Parse_error ->
             (print_string "\ndoes not parse\n";
              loop gamma))
  with Picomllex.EndInput -> exit 0
 in loop []
