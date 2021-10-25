(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Divide
  | Mod
  | Equals
  | GreaterEqual
  | GreaterThan
  | LessEqual
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
let rec free_vars (exp : expr) : varidset = 
  match exp with
  | Var v -> SS.add v SS.empty
  | Unop(_, e) -> free_vars e 
  | App(e1, e2)
  | Binop(_, e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | Conditional(e1, e2, e3) -> free_vars e1 |> SS.union (free_vars e2) 
                                            |> SS.union (free_vars e3)
  | Fun(v, e) -> SS.remove v (free_vars e)
  | Let(v, e1, e2) -> SS.union (SS.remove v (free_vars e2)) (free_vars e1)
  | Letrec(v, e1, e2) -> SS.union (SS.remove v (free_vars e2)) 
                                  (SS.remove v (free_vars e1))
  | _ -> SS.empty ;;
  
(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)

let new_varname =
  let ctr = ref 0 in
  let temp = ref 0 in 
  fun () : varid -> 
    temp := !ctr;
    ctr := !ctr + 1;
    "var" ^ string_of_int !temp ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  
  (* Helper partially applied function that substitutes the same
     var_name for repl in a different exp each time it is applied *)
  let sub = subst var_name repl in

  (* Helper that replaces a variable v for a 
     new, temporary variable temp in expression e *)
  let replnew v temp e  = sub (subst v (Var temp) e) in
  match exp with
   | Var v -> if v = var_name then repl else Var v
   | Num i -> Num i
   | Bool b -> Bool b
   | Unop(u, e) -> Unop(u, sub e)
   | Binop(b, e1, e2) -> Binop(b, sub e1, sub e2)
   | Conditional(e1, e2, e3) -> Conditional(sub e1, sub e2, sub e3)
   | Fun(v, e) -> if v = var_name then Fun(v, e)
                  else (if SS.mem v (free_vars repl) then 
                          let temp = new_varname () in 
                          Fun(temp,  replnew v temp e)
                        else Fun(v, sub e))  
   | Let(v, e1, e2) -> if v = var_name then Let(v, sub e1, e2)
                       else (if SS.mem v (free_vars repl) then 
                               let temp = new_varname () in 
                               Let(temp, sub e1, replnew v temp e2)
                             else Let(v, sub e1, sub e2))
   | Letrec(v, e1, e2) -> if v = var_name then Letrec(v, e1, e2)
                          else (if SS.mem v (free_vars repl) then 
                                  let temp = new_varname () in 
                                  Letrec(temp, replnew v temp e1, 
                                         replnew v temp e2)
                                else Letrec(v, sub e1, sub e2))
   | Raise -> Raise
   | Unassigned -> Unassigned
   | App(e1, e2) -> App(sub e1, sub e2) ;;
  



(*......................................................................
  String representations of expressions
 *)
   
    
(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var v -> v
  | Num i -> string_of_int i
  | Bool b -> string_of_bool b
  | Unop(u, e) -> let u_to_string = (match u with
                                     | Negate -> "-") in
                  u_to_string ^ "[" ^ exp_to_concrete_string e ^ "]"
  | Binop(b, e1, e2) -> let b_to_string = (match b with
  						                             | Plus -> " + "
						                               | Minus -> " - "
						                               | Times -> " * "
                                           | Divide -> " / "
                                           | Mod -> " mod "
						                               | Equals -> " = "
                                           | GreaterEqual -> " >= "
                                           | LessEqual -> " <= "
                                           | GreaterThan -> " > "
						                               | LessThan -> " < ") in
                        "[" ^ exp_to_concrete_string e1 ^ b_to_string 
                            ^ exp_to_concrete_string e2 ^ "]"
  | Conditional(e1, e2, e3) -> "[if " ^ exp_to_concrete_string e1  
                               ^ " then " ^ exp_to_concrete_string e2
                               ^ " else " ^ exp_to_concrete_string e3 ^ "]"
  | Fun(v, e) -> "[function " ^ v ^ " -> " ^ exp_to_concrete_string e ^ "]"
  | Let(v, e1, e2) -> "[let " ^ v ^ " = " ^ exp_to_concrete_string e1
                      ^ " in " ^ exp_to_concrete_string e2 ^ "]"
  | Letrec(v, e1, e2) -> "[let rec " ^ v ^ " = " ^ exp_to_concrete_string e1
                         ^ " in " ^ exp_to_concrete_string e2 ^ "]"
  | Raise -> "raise" (* ?? *)
  | Unassigned -> "?" (* (temporarily) unassigned ?? *)
  | App(e1, e2) -> "[" ^ exp_to_concrete_string e1 ^ " " 
                   ^ exp_to_concrete_string e2 ^ "]";;

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var v -> "Var(" ^ v ^ ")"
  | Num i -> "Num(" ^ string_of_int i ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Unop(u, e) -> let u_to_string = (match u with
                                     | Negate -> "Negate") in
                  "Unop(" ^ u_to_string ^ ", " ^ exp_to_abstract_string e ^ ")"
  | Binop(b, e1, e2) ->  let b_to_string = (match b with
                                            | Plus -> "Plus"
                                            | Minus -> "Minus"
                                            | Times -> "Times"
                                            | Divide -> "Divide"
                                            | Mod -> "Mod"
                                            | Equals -> "Equals"
                                            | GreaterEqual -> "GreaterEqual"
                                            | LessEqual -> "LessEqual"
                                            | GreaterThan -> "GreaterThan"
                                            | LessThan -> "LessThan") in
                         "Binop(" ^ b_to_string ^ ", "  
                         ^ exp_to_abstract_string e1 ^ ", " 
                         ^ exp_to_abstract_string e2 ^ ")"
  | Conditional(e1, e2, e3) -> "Conditional(" ^ exp_to_abstract_string e1 ^ ", "
                               ^ exp_to_abstract_string e2 ^ ", "
                               ^ exp_to_abstract_string e3 ^ ")"
  | Fun(v, e) -> "Fun(" ^ v ^ ", " ^ exp_to_abstract_string e ^ ")"
  | Let(v, e1, e2) -> "Let(" ^ v ^ ", " ^ exp_to_abstract_string e1 ^ ", "
                      ^ exp_to_abstract_string e2 ^ ")"
  | Letrec(v, e1, e2) -> "Letrec(" ^ v ^ ", " 
                         ^ exp_to_abstract_string e1 ^ ", " 
                         ^ exp_to_abstract_string e2 ^ ")"
  | Raise -> "Raise" (* ?? *)
  | Unassigned -> "Unassigned" (* ?? *)
  | App(e1, e2) -> "App(" ^ exp_to_abstract_string e1 ^ ", "
                   ^ exp_to_abstract_string e2 ^ ")" ;;
 