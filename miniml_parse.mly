/*
                         CS 51 Final Project
                           MiniML -- Parser
*/
                  
%{
  open Expr ;;
%}

%token EOF
%token OPEN CLOSE
%token LET DOT IN REC
%token NEG
%token PLUS MINUS 
%token TIMES DIVIDE MOD
%token LESSTHAN EQUALS GREATERTHAN GEQ LEQ
%token IF THEN ELSE 
%token FUNCTION
%token RAISE
%token <string> ID
%token <int> INT 
%token TRUE FALSE

%right LESSTHAN GREATERTHAN GEQ LEQ EQUALS
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left NEG

%start input
%type <Expr.expr> input

/* Grammar follows */
%%
input:  exp EOF                 { $1 }

exp:    exp expnoapp            { App($1, $2) }
        | expnoapp              { $1 }

expnoapp: INT                   { Num $1 }
        | TRUE                  { Bool true }
        | FALSE                 { Bool false }
        | ID                    { Var $1 }
        | exp PLUS exp          { Binop(Plus, $1, $3) }
        | exp MINUS exp         { Binop(Minus, $1, $3) }
        | exp TIMES exp         { Binop(Times, $1, $3) }
        | exp DIVIDE exp        { Binop(Divide, $1, $3) }
        | exp MOD exp           { Binop(Mod, $1, $3) }
        | exp EQUALS exp        { Binop(Equals, $1, $3) }
        | exp LESSTHAN exp      { Binop(LessThan, $1, $3) }
        | exp GREATERTHAN exp   { Binop(GreaterThan, $1, $3) }
        | exp LEQ exp           { Binop(LessEqual, $1, $3) }
        | exp GEQ exp           { Binop(GreaterEqual, $1, $3) }
        | NEG exp               { Unop(Negate, $2) }
        | IF exp THEN exp ELSE exp      { Conditional($2, $4, $6) }
        | LET ID suglet IN exp          { Let($2, $3, $5) }
        | LET REC ID suglet IN exp      { Letrec($3, $4, $6) }
        | FUNCTION ID sugfun            { Fun($2, $3) } 
        | RAISE                 { Raise }
        | OPEN exp CLOSE        { $2 }

sugfun: ID sugfun                  { Fun($1, $2) }
        | DOT exp                  { $2 } 

suglet: ID suglet                  { Fun($1, $2) }
        | EQUALS exp               { $2 } 
;

%%
