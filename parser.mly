/* Simple lambda calculus */

%{
open Printf
open Lexing
open Stlc_types

let var_table = Hashtbl.create 16
%}

%token <int> NUM
%token LPAREN
%token RPAREN
%token PLUS
%token MINUS
%token MULT
%token EOF
%token ARROW
%token DBLARROW
%token COLON
%token EMPTYPAREN
%token EQUAL
%token LT

%token FUNCTION_KW
%token LET_KW
%token LETREC_KW
%token IN_KW
%token IF_KW
%token THEN_KW
%token ELSE_KW
%token TRUE_KW
%token FALSE_KW
%token UNIT_T_KW
%token INT_T_KW
%token BOOL_T_KW

%token <string> IDENT

%left PLUS MINUS
%left MULT

%start <Stlc_types.value option> input
/* Grammar folows */
%%

input:
 | EOF      { None }
 | t = term EOF { Some t }
;

term:
 | t1 = term; t2 = term_nt { Funcall(t1, t2) }
 | term_nt { $1 }
;
term_nt:
 | TRUE_KW { Bool true}
 | FALSE_KW { Bool false}
 | EMPTYPAREN { Unit }

 | i = NUM  { Int i }
 | v = var { Var v }

 | LPAREN; t=term; RPAREN { t }

 | IF_KW; cond=term; THEN_KW; cl1=term; ELSE_KW; cl2=term
   { If (cond, cl1, cl2) }

 | FUNCTION_KW; id = var; COLON; annot=typ; DBLARROW; body=term
 { Function (id, annot, body) }

 | LET_KW; id = var; EQUAL; v=term; IN_KW; e=term { Let (id, v, e) }

 | LETREC_KW; id = var; COLON; annot=typ; EQUAL; v=term; IN_KW; e=term { Letrec (id, annot, v, e) }

 | t1=term; PLUS; t2=term { Plus (t1, t2) }

 | t1=term; MINUS; t2=term { Minus (t1, t2) }

 | t1=term; MULT; t2=term { Mult (t1, t2) }

 | t1=term; LT; t2=term { Lt (t1, t2) }
;


var:
 | x = IDENT { x }


typ:
 | UNIT_T_KW { UnitT }
 | INT_T_KW  { IntT }
 | BOOL_T_KW  { BoolT }
 | LPAREN; t=typ; RPAREN { t }
 | t1=typ; ARROW; t2=typ { Arrow (t1, t2) }
;
