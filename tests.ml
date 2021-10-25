(* UNIT TESTS *)
open Absbook
open Expr
open Evaluation
module Ev = Evaluation ;;

(*Some expressions*)
let var = Var("x") ;;
let num = Num(2) ;;
let boolean = Bool(true) ;;
let unop = Unop(Negate, Var("y")) ;;
let b_plus = Binop(Plus, Var("y"), Var("k")) ;;
let b_minus = Binop(Minus, Var("y"), Num(3)) ;;
let b_times = Binop(Times, Num(5), Var("y")) ;;
let b_equals =  Binop(Equals, Var("y"), Num(3)) ;;
let b_lessthan = Binop(LessThan, Var("y"), Num(3)) ;;
let b_greaterthan = Binop(GreaterThan, Var("y"), Num(3)) ;;
let b_geq = Binop(GreaterEqual, Var("y"), Num(3)) ;;
let b_leq = Binop(LessEqual, Var("y"), Num(3)) ;;
let b_divide = Binop(Divide, Var("y"), Num(3)) ;;
let b_mod = Binop(Mod, Var("y"), Num(3)) ;;
let condition = Conditional(Binop(Equals, Var("y"), Num(5)), Binop(Times, Num(5), Var("x")), Binop(Minus, Var("z"), Num(3))) ;;
let funct = Fun("z", Unop(Negate, Var("z"))) ;;
let letexp = Let("z", Unop(Negate, Num(6)), Binop(LessThan, Var("z"), Num(3))) ;;
let letrecexp = Letrec("f", Fun("x", App(Var("f"), Binop(Minus, Var("x"), Num(1)))), App(Var("f"), Num(3))) ;;
let raiseexp = Raise ;;
let unassignedexp = Unassigned ;;
let application = App(Fun("z", Unop(Negate, Var("z"))), Num(3)) ;;
let letrecexp2 = Letrec("f", Fun("x", Binop(Plus,App(Var("f"), Var("x")), Var("z"))), Var("y")) ;;
let factorial = Letrec("fact", Fun("x", Conditional(Binop(LessThan, Var("x"), Num(2)), Num(1), Binop(Times, Var("x"), App(Var("fact"), Binop(Minus, Var("x"), Num(1)))))), App(Var("fact"), Num(5))) ;;
let fibonacci = Letrec("fib", Fun("x", Conditional(Binop(LessThan, Var("x"), Num(2)), Num(1), Binop(Plus, App(Var("fib"), Binop(Minus, Var("x"), Num(1))), App(Var("fib"), Binop(Minus, Var("x"), Num(2)))))), App(Var("fib"), Num(5))) ;;
let multivariable =  Let("x", Num(10), Let("f", Fun("y", Fun("z", Binop(Times, Var("z"), Binop(Plus, Var("x"), Var("y"))))), Let("y", Num(12), App(App(Var("f"), Num(11)), Num(2))))) ;;

(*Some refs*)
let ref1 = ref (Ev.Env.Val(num)) ;;
let ref2 = ref (Ev.Env.Val(funct)) ;;
let ref3 = ref (Ev.Env.Val(boolean)) ;;
let ref4 = ref (Ev.Env.Val(boolean)) ;;

(*Some environments*)
let empty_environment = Ev.Env.empty ()
let environment1 = Ev.Env.extend (Ev.Env.empty ()) "x" ref1 ;;
let environment2 = Ev.Env.extend (Ev.Env.empty ()) "x" ref2 ;;
let environment3 = Ev.Env.extend environment2 "y" ref3 ;;
let environment4 = Ev.Env.extend environment3 "z" ref4 ;;
let environment5 = Ev.Env.extend environment3 "y" ref1 ;;
let environment_replaced = Ev.Env.extend environment4 "z" ref1 ;;
let environment_replaced2 = Ev.Env.extend environment4 "x" ref2 ;;
let environment_replaced3 = Ev.Env.extend environment4 "z" ref2 ;;
let environment_replaced4 = Ev.Env.extend environment4 "z" ref4 ;;

(* expr.ml Tests *)
let new_varname_test () = 
  Printf.printf "Evaluating new_varname Tests\n";
  unit_test (new_varname () = "var0") "variable 0";
  unit_test (new_varname () = "var1") "variable 1";
  unit_test (new_varname () = "var2") "variable 2";
  unit_test (new_varname () = "var3") "variable 3";
  print_newline () ;;

let exp_to_concrete_string_test () = 
  Printf.printf "Evaluating exp_to_concrete_string Tests\n";
  unit_test (exp_to_concrete_string var = "x") "variable";
  unit_test (exp_to_concrete_string num = "2") "number";
  unit_test (exp_to_concrete_string boolean = "true") "boolean";
  unit_test (exp_to_concrete_string unop = "-[y]") "Unop";
  unit_test (exp_to_concrete_string b_plus = "[y + k]") "Binop plus var var";
  unit_test (exp_to_concrete_string b_minus = "[y - 3]") "Binop minus var num";
  unit_test (exp_to_concrete_string b_times = "[5 * y]") "Binop times var num";
  unit_test (exp_to_concrete_string b_equals = "[y = 3]") "Binop equals var num";
  unit_test (exp_to_concrete_string b_lessthan = "[y < 3]") "Binop lessthan var num";
  unit_test (exp_to_concrete_string b_leq = "[y <= 3]") "Binop lessequal var num";
  unit_test (exp_to_concrete_string b_geq = "[y >= 3]") "Binop greaterequal var num";
  unit_test (exp_to_concrete_string b_divide = "[y / 3]") "Binop divide var num";
  unit_test (exp_to_concrete_string b_mod = "[y mod 3]") "Binop mod var num";
  unit_test (exp_to_concrete_string b_greaterthan = "[y > 3]") "Binop GreaterThan var num";
  unit_test (exp_to_concrete_string condition = "[if [y = 5] then [5 * x] else [z - 3]]") "conditional";
  unit_test (exp_to_concrete_string funct = "[function z -> -[z]]") "fun";
  unit_test (exp_to_concrete_string letexp = "[let z = -[6] in [z < 3]]") "let";
  unit_test (exp_to_concrete_string letrecexp = "[let rec f = [function x -> [f [x - 1]]] in [f 3]]") "letrec";
  unit_test (exp_to_concrete_string raiseexp = "raise") "raise";
  unit_test (exp_to_concrete_string unassignedexp = "?") "unassigned";
  unit_test (exp_to_concrete_string application = "[[function z -> -[z]] 3]") "app";
  print_newline () ;;

let exp_to_abstract_string_test () = 
  Printf.printf "Evaluating exp_to_abstract_string Tests\n";
  unit_test (exp_to_abstract_string var = "Var(x)") "variable";
  unit_test (exp_to_abstract_string num = "Num(2)") "number";
  unit_test (exp_to_abstract_string boolean = "Bool(true)") "boolean";
  unit_test (exp_to_abstract_string unop = "Unop(Negate, Var(y))") "Unop";
  unit_test (exp_to_abstract_string b_plus = "Binop(Plus, Var(y), Var(k))") "Binop plus var var";
  unit_test (exp_to_abstract_string b_minus = "Binop(Minus, Var(y), Num(3))") "Binop minus var num";
  unit_test (exp_to_abstract_string b_times = "Binop(Times, Num(5), Var(y))") "Binop times var num";
  unit_test (exp_to_abstract_string b_equals = "Binop(Equals, Var(y), Num(3))") "Binop equals var num";
  unit_test (exp_to_abstract_string b_lessthan = "Binop(LessThan, Var(y), Num(3))") "Binop lessthan var num";
  unit_test (exp_to_abstract_string b_leq = "Binop(LessEqual, Var(y), Num(3))") "Binop lessequal var num";
  unit_test (exp_to_abstract_string b_geq = "Binop(GreaterEqual, Var(y), Num(3))") "Binop greaterequal var num";
  unit_test (exp_to_abstract_string b_divide = "Binop(Divide, Var(y), Num(3))") "Binop divide var num";
  unit_test (exp_to_abstract_string b_mod = "Binop(Mod, Var(y), Num(3))") "Binop mod var num";
  unit_test (exp_to_abstract_string b_greaterthan = "Binop(GreaterThan, Var(y), Num(3))") "Binop GreaterThan var num";
  unit_test (exp_to_abstract_string condition = "Conditional(Binop(Equals, Var(y), Num(5)), Binop(Times, Num(5), Var(x)), Binop(Minus, Var(z), Num(3)))") "conditional";
  unit_test (exp_to_abstract_string funct = "Fun(z, Unop(Negate, Var(z)))") "fun";
  unit_test (exp_to_abstract_string letexp = "Let(z, Unop(Negate, Num(6)), Binop(LessThan, Var(z), Num(3)))") "let";
  unit_test (exp_to_abstract_string letrecexp = "Letrec(f, Fun(x, App(Var(f), Binop(Minus, Var(x), Num(1)))), App(Var(f), Num(3)))") "letrec";
  unit_test (exp_to_abstract_string raiseexp = "Raise") "raise";
  unit_test (exp_to_abstract_string unassignedexp = "Unassigned") "unassigned";
  unit_test (exp_to_abstract_string application = "App(Fun(z, Unop(Negate, Var(z))), Num(3))") "app";
  unit_test (exp_to_abstract_string factorial = "Letrec(fact, Fun(x, Conditional(Binop(LessThan, Var(x), Num(2)), Num(1), Binop(Times, Var(x), App(Var(fact), Binop(Minus, Var(x), Num(1)))))), App(Var(fact), Num(5)))") "factorial";
  print_newline () ;;

let free_vars_test () = 
  Printf.printf "Evaluating free_vars Tests\n";
  unit_test (same_vars (free_vars var) (vars_of_list ["x"])) "variable";
  unit_test (free_vars num = vars_of_list []) "number";
  unit_test (free_vars boolean = vars_of_list []) "boolean";
  unit_test (free_vars unop = vars_of_list ["y"]) "Unop";
  unit_test (free_vars b_plus = vars_of_list ["y"; "k"]) "Binop plus var num";
  unit_test (free_vars b_minus = vars_of_list ["y"]) "Binop minus var num";
  unit_test (free_vars b_times = vars_of_list ["y"]) "Binop times var num";
  unit_test (free_vars b_equals = vars_of_list ["y"]) "Binop equals var num";
  unit_test (free_vars b_lessthan = vars_of_list ["y"]) "Binop lessthan var num";
  unit_test (free_vars b_mod = vars_of_list ["y"]) "Binop lessthan var num";
  unit_test (free_vars b_greaterthan = vars_of_list ["y"]) "Binop GreaterThan var num";
  unit_test (free_vars condition = vars_of_list ["x"; "y"; "z"]) "conditional";
  unit_test (free_vars funct = vars_of_list []) "fun empty";
  unit_test (free_vars (Fun("y", b_plus)) = vars_of_list ["k"]) "fun notempty";
  unit_test (free_vars letexp = vars_of_list []) "let empty";
  unit_test (free_vars (Let("z", b_times, b_plus)) = vars_of_list ["y"; "k"]) "let notempty";
  unit_test (free_vars letrecexp = vars_of_list []) "letrec";
  unit_test (free_vars raiseexp = vars_of_list []) "raise";
  unit_test (free_vars unassignedexp = vars_of_list []) "?";
  unit_test (free_vars application = vars_of_list []) "app";
  print_newline () ;;

let subst_test () = 
  Printf.printf "Evaluating subst Tests\n";
  unit_test (subst "x" num var = num) "variable for num";
  unit_test (subst "y" num var = var) "variable no change";
  unit_test (subst "x" (Var "z") num = num) "number";
  unit_test (subst "x" (Var "z") boolean = boolean) "boolean";
  unit_test (subst "x" num unop = unop) "Unop no change";
  unit_test (subst "y" num unop = Unop(Negate, num)) "Unop change";
  unit_test (subst "z" num b_plus = b_plus ) "Binop plus no change";
  unit_test (subst "k" unop b_plus = Binop(Plus, Var("y"), unop)) "Binop plus no change";    
  unit_test (subst "y" var b_minus = Binop(Minus, Var("x"), Num(3))) "Binop minus replace";
  unit_test (subst "j" num b_times = b_times) "Binop times no change";
  unit_test (subst "j" num b_equals = b_equals) "Binop equals no change";
  unit_test (subst "j" num b_lessthan = b_lessthan) "Binop lessthan no change";
  unit_test (subst "j" num b_greaterthan = b_greaterthan) "Binop GreaterThan no change";
  unit_test (subst "z" num condition = Conditional(Binop(Equals, Var("y"), Num(5)), Binop(Times, Num(5), Var("x")), Binop(Minus, num, Num(3))) ) "conditional";
  unit_test (subst "z" num funct = funct) "fun should be no change";
  unit_test (subst "y" num (Fun("y", b_plus)) = Fun("y", b_plus)) "fun no change";
  unit_test (subst "k" num (Fun("y", b_plus)) = Fun("y", Binop(Plus, Var("y"), num))) "fun should be change";
  unit_test (subst "z" num letexp = letexp) "let empty no change";
  unit_test (subst "y" num (Let("z", b_times, b_plus)) = Let("z", Binop(Times, Num(5), num), Binop(Plus, num, Var("k")))) "let change";
  unit_test (subst "y" var (Let("x", Num(5), b_plus)) = Let("var4", Num(5), Binop(Plus, Var("x"), Var("k")))) "let use fresh var";
  unit_test (subst "f" var letrecexp = letrecexp) "letrec no change 1";
  unit_test (subst "f" num letrecexp = letrecexp) "letrec no change 2";
  unit_test (subst "x" var letrecexp = letrecexp) "letrec no change 1";
  unit_test (subst "z" num letrecexp2 = Letrec("f", Fun("x", Binop(Plus,App(Var("f"), Var("x")), num)), Var("y"))) "letrec2 change 1";
  unit_test (subst "y" num letrecexp2 = Letrec("f", Fun("x", Binop(Plus,App(Var("f"), Var("x")), Var("z"))), num)) "letrec2 change 2";
  unit_test (subst "y" (Var "f") letrecexp2 = Letrec("var5", Fun("x", Binop(Plus,App(Var("var5"), Var("x")), Var("z"))), Var("f"))) "letrec2 change 3";
  unit_test (subst "y" num raiseexp = Raise) "raise";
  unit_test (subst "y" num unassignedexp = Unassigned) "unassigned";
  unit_test (subst "z" num application = application) "app should have no change";
  unit_test (subst "f" num (App(Var("f"), Num(3))) = App(num, Num(3))) "app should have change";
  print_newline () ;;

(* evaluate.ml Tests *)
(* evaluate.ml Tests helper functions *)
type semantics = D | S | L

let eval_test_extractor (value : Ev.Env.value) : expr = 
  match value with 
  | Val v -> v
  | Closure (v, _) -> v ;;

let eval_test_helper (s : semantics) (exp : expr) : expr =
  let sem = (match s with
             | D -> eval_d 
             | S -> eval_s
             | L -> eval_l) in
  eval_test_extractor (sem exp empty_environment) ;;

let eval_s_tests () = 
  Printf.printf "Evaluating eval_s Tests\n";
  unit_test (eval_test_helper S num = num) "number";
  unit_test (eval_test_helper S boolean = boolean) "boolean";
  unit_test (eval_test_helper S (App(funct, num)) = Num(-2)) "fun app";
  unit_test (eval_test_helper S letexp = Bool(true)) "let";
  unit_test (eval_test_helper S factorial = Num(120)) "let rec factorial";
  unit_test (eval_test_helper S (Binop(Plus, num, num)) = Num(4)) "Binop plus 2num";
  unit_test (eval_test_helper S (Binop(Minus, num, num)) = Num(0)) "Binop minus 2num";
  unit_test (eval_test_helper S (Binop(Times, num, num)) = Num(4)) "Binop times 2num";
  unit_test (eval_test_helper S (Binop(Divide, num, num)) = Num(1)) "Binop divide 2num";
  unit_test (eval_test_helper S (Binop(Equals, num, num)) = Bool(true)) "Binop equals true";
  unit_test (eval_test_helper S (Binop(Equals, num, Num(5))) = Bool(false)) "Binop equals false";
  unit_test (eval_test_helper S (Binop(LessThan, num, num)) = Bool(false)) "Binop lessthan false";
  unit_test (eval_test_helper S (Binop(LessThan, num, Num(5))) = Bool(true)) "Binop lessthan true";
  unit_test (eval_test_helper S (Binop(LessEqual, num, Num(5))) = Bool(true)) "Binop lessequal true";
  unit_test (eval_test_helper S (Binop(Mod, num, Num(5))) = Num(2)) "Binop mod";
  unit_test (eval_test_helper S (Binop(Divide, num, Num(5))) = Num(0)) "Binop divide";
  unit_test (eval_test_helper S (Conditional(Bool(true), num, Num(5))) = num) "conditional true";
  unit_test (eval_test_helper S fibonacci = Num(8)) "fibonacci";
  unit_test (eval_test_helper S multivariable = Num(42)) "multivariable";
  print_newline () ;;

let eval_d_tests () = 
  Printf.printf "Evaluating eval_d Tests\n";
  unit_test (eval_test_helper D num = num) "number";
  unit_test (eval_test_helper D boolean = boolean) "boolean";
  unit_test (eval_test_helper D (Binop(Plus, num, num)) = Num(4)) "Binop plus 2num";
  unit_test (eval_test_helper D (Binop(Minus, num, num)) = Num(0)) "Binop minus 2num";
  unit_test (eval_test_helper D (Binop(Times, num, num)) = Num(4)) "Binop times 2num";
  unit_test (eval_test_helper D (Binop(Equals, num, num)) = Bool(true)) "Binop equals true";
  unit_test (eval_test_helper D (Binop(Equals, num, Num(5))) = Bool(false)) "Binop equals false";
  unit_test (eval_test_helper D (Binop(LessThan, num, num)) = Bool(false)) "Binop lessthan false";
  unit_test (eval_test_helper D (Binop(LessThan, num, Num(5))) = Bool(true)) "Binop lessthan true";
  unit_test (eval_test_helper D (Conditional(Bool(true), num, Num(5))) = num) "conditional true";
  unit_test (eval_test_helper D (App(funct, num)) = Num(-2)) "fun app";
  unit_test (eval_test_helper D letexp = Bool(true)) "let";
  unit_test (eval_test_helper D factorial = Num(120)) "let rec factorial";
  unit_test (eval_test_helper D fibonacci = Num(8)) "fibonacci";
  unit_test (eval_test_helper D multivariable = Num(44)) "multivariable";
  unit_test (eval_test_extractor (eval_d funct environment1) = funct) "evaluate funct in env1";
  unit_test (eval_test_extractor (eval_d var environment1) = Num(2)) "evaluate var in env1";
  unit_test (eval_test_extractor (eval_d funct environment3) = funct) "evaluate funct in env3";
  unit_test (eval_test_extractor (eval_d b_times environment5) = Num(10)) "evaluate b_times in env5";
  print_newline () ;;

let helper_functions_tests () = 
  Printf.printf "Evaluating eval_d Tests\n";
  unit_test (extract_value (Ev.Env.Val(condition)) = condition) "Extract value val1";
  unit_test (extract_value (Ev.Env.Val(factorial)) = factorial) "Extract value val2";
  unit_test (extract_value (Ev.Env.Val(num)) = num) "Extract value val3";
  unit_test (extract_value (Ev.Env.Closure(var, Ev.Env.empty ())) = var) "Extract value closure1";
  unit_test (extract_value (Ev.Env.Closure(fibonacci, Ev.Env.empty ())) = fibonacci) "Extract value closure2";
  unit_test (extract_value (Ev.Env.Closure(multivariable, Ev.Env.empty ())) = multivariable) "Extract value closure3";
  unit_test (extract_num num = 2) "Extract num";
  unit_test (extract_bool boolean = true) "Extract boolean";
  print_newline () ;;

let module_Env_tests () = 
  Printf.printf "Evaluating Env module Tests\n";
  unit_test (Ev.Env.lookup environment1 "x" = Ev.Env.lookup environment_replaced "z" ) "Lookup - diff env diff var same val";
  unit_test (Ev.Env.lookup environment2 "x" = Ev.Env.lookup environment_replaced2 "x") "Lookup - diff env same var same val";
  unit_test (Ev.Env.lookup environment3 "x" = Ev.Env.lookup environment2 "x") "Lookup - environment is extension of other same all";
  unit_test (Ev.Env.lookup environment4 "z" <> Ev.Env.lookup environment_replaced3 "z") "Lookup - replaced variable, should be different";
  unit_test (environment_replaced4 = environment4) "Extend - extended same thing";
  unit_test (Ev.Env.close multivariable environment4 = Ev.Env.close multivariable environment4) "Close to equal environments";
  unit_test (Ev.Env.env_to_string environment1 = "[x |-> 2]") "Environment to string num";
  unit_test (Ev.Env.env_to_string environment2 = "[x |-> " ^ exp_to_concrete_string funct ^ "]") "Environment to string fun";
  unit_test (Ev.Env.env_to_string environment3 = "[y |-> true; x |-> " ^ exp_to_concrete_string funct ^ "]") "Environment to string bool";
  unit_test (Ev.Env.env_to_string environment4 = "[z |-> true; y |-> true; x |-> " ^ exp_to_concrete_string funct ^ "]") "Environment to string bool and fun";
  unit_test (Ev.Env.env_to_string environment_replaced4 = "[z |-> true; y |-> true; x |-> " ^ exp_to_concrete_string funct ^ "]") "Environment to string replaced equal environment4";
  unit_test (Ev.Env.env_to_string environment_replaced4 = "[z |-> true; y |-> true; x |-> " ^ exp_to_concrete_string funct ^ "]") "Environment replaced equal environment4";
  unit_test (Ev.Env.value_to_string !ref2 = exp_to_concrete_string funct) "ref2 to string";
  unit_test (Ev.Env.value_to_string !ref1 = exp_to_concrete_string num) "ref1 to string";
  unit_test (Ev.Env.value_to_string !ref3 = exp_to_concrete_string boolean) "ref3 to string";
  unit_test (Ev.Env.value_to_string (Ev.Env.Closure(funct, environment4)) = exp_to_concrete_string funct ^ " where " ^ Ev.Env.env_to_string environment_replaced4) "Closure1 to string";
  unit_test (Ev.Env.value_to_string (Ev.Env.Closure(multivariable, environment5)) = exp_to_concrete_string multivariable ^ " where " ^ Ev.Env.env_to_string environment5) "Closure2 to string";
  unit_test (Ev.Env.value_to_string (Ev.Env.Closure(factorial, environment_replaced3)) = exp_to_concrete_string factorial ^ " where " ^ Ev.Env.env_to_string environment_replaced3) "Closure3 to string";
  print_newline () ;;

(*evaluate all*)
let _ =
  print_newline ();
  new_varname_test ();
  exp_to_concrete_string_test ();
  exp_to_abstract_string_test ();
  free_vars_test ();
  subst_test ();
  eval_s_tests ();
  eval_d_tests ();
  helper_functions_tests ();
  module_Env_tests ();

