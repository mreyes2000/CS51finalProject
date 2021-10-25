(* 
                         CS 51 Final Project
                    MiniML -- Read-Eval-Print Loop
 *)

module Ev = Evaluation ;;
module MP = Miniml_parse ;;
module ML = Miniml_lex ;;
module Ex = Expr ;;


open Printf ;;

(* str_to_exp : string -> expr
   Return the expression specified by the string using the Miniml
   parser. *)
let str_to_exp (str: string) : Ex.expr =
  let lexbuf = Lexing.from_string str in
  let exp = MP.input ML.token lexbuf in
  exp ;;

(* repl : unit -> unit
   Read-eval-print loop for MiniML. *)
let repl () =
  (* lexical analyzer buffer from stdin *)
  let lexbuf = Lexing.from_channel stdin in
  (* set up the initial environment *)
  let env = Ev.Env.empty () in
  printf "Entering %s...\n" Sys.argv.(0);
  flush stdout;
  while true do
    (try
        (* prompt *)
        printf "<== ";
        flush stdout;
        
        (* read and parse an expression from the input *)
        let exp = MP.input ML.token lexbuf in

        (* prints the abstract string of the parsed expression *)
        printf "--> %s\n" (Ex.exp_to_abstract_string exp);
        
        (* prints the concrete string of the result after evaluated
           using the substitution semantics. If the evaluation leads
           to an error, it catches it, displays it, and proceeds
           with the dynamic semantics *)
        (try (let substitution = Ev.eval_s exp env in
              printf "s=> %s\n" (Ev.Env.value_to_string substitution)) with
         | Ev.EvalError msg -> printf "sx> evaluation error: %s\n" msg);
        
        (* prints the concrete string of the result after evaluated
           using the dynamic semantics. If the evaluation leads
           to an error, it catches it, displays it, and proceeds *)
        (try (let dynamical = Ev.eval_d exp env in
             printf "d=> %s\n" (Ev.Env.value_to_string dynamical)) with
         | Ev.EvalError msg -> printf "dx> evaluation error: %s\n" msg)
      
      (* Some common errors that may occur in addition
         to the evaluation errors in each semantics *)     
      with
      | Parsing.Parse_error -> printf "xx> parse error\n"
      | Ev.EvalError msg -> printf "xx> evaluation error: %s\n" msg
      | Ev.EvalException -> printf "xx> evaluation exception\n"
      | End_of_file -> printf "Goodbye.\n"; exit 0
    );
    flush stdout
  done
;;
        
(* Run REPL if called from command line *)

try
  let _ = Str.search_forward (Str.regexp "miniml\\.\\(byte\\|native\\)") (Sys.argv.(0)) 0 in
  repl ()
with Not_found -> () ;;
