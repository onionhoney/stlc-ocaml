/* Simple lambda calculus */

%{
open Printf
open Lexing

let var_table = Hashtbl.create 16
%}

%token <int> NUM
%token LPAREN
%token RPAREN
%token PLUS
%token MULT
%token EOF
%token ARROW
%token DBLARROW
%token COLON
%token EMPTYPAREN
%token EQUAL

%token FUNCTION_KW
%token LET_KW
%token IN_KW
%token UNIT_T_KW
%token INT_T_KW

%token <string> IDENT

%left PLUS
%left MULT

%start <Typedlambda.value option> input
/* Grammar folows */
%%

input:
 | EOF      { None }
 | t = term EOF { Some t }
;
 
term:
 | i = NUM  { `Int i }
 | EMPTYPAREN { `Unit }
 | x = IDENT { `Var x }
 | LPAREN; t=term; RPAREN { t }
 | FUNCTION_KW; id=IDENT; COLON; annot=typ; DBLARROW; body=term
 { `Function (id, annot, body) }

 | LET_KW; id=IDENT; EQUAL; v=term; IN_KW; e=term
 { `Let (id, v, e) }
 | t1=term; t2=term { `Funcall (t1, t2) }
 | t1=term; PLUS; t2=term { `Plus (t1, t2) }
 | t1=term; MULT; t2=term { `Mult (t1, t2) }
 
;


typ:
 | UNIT_T_KW { `UnitT } 
 | INT_T_KW  { `IntT }
 | LPAREN; t=typ; RPAREN { t }
 | t1=typ; ARROW; t2=typ { `Arrow (t1, t2) }  
;
