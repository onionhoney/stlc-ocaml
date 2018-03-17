/* Simple lambda calculus */

%{
open Printf
open Lexing
open Stlc_types

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
%token COMMA
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

%token < Stlc_types.value -> Stlc_types.value > TUPLE_KWS
 /*
%token FST_KW
%token SND_KW
%token FST3_KW
%token SND3_KW
%token TRD3_KW
 */

%token <string> IDENT

%left PLUS MINUS
%left MULT

%start <Stlc_types.value option> input
/* Grammar folows */
%%

input:
 | EOF      { None }
 | t = block EOF { Some t }
;

block:
 | LET_KW; id = var; EQUAL; v=call; IN_KW; e=call { Let (id, v, e) }
 | LETREC_KW; id = var; COLON; annot=typ; EQUAL; v=call; IN_KW; e=call { Letrec (id, annot, v, e) }
 | call { $1 }
;

call:
  | kw = TUPLE_KWS; t = term { kw t (*try (tuple_kw_find kw) t with Not_found -> t *) }
  | t1 = term; t2 = term { Funcall(t1, t2) }
  | term { $1 }

term:
  | LPAREN; t1 = call; COMMA; t2 = call; COMMA; t3 = call; RPAREN { Tuple3 (t1, t2, t3) }
  | LPAREN; t1 = call; COMMA; t2 = call; RPAREN { Tuple2 (t1, t2) }
  | term_nt { $1 }
;

term_nt:
 | TRUE_KW { Bool true}
 | FALSE_KW { Bool false}
 | EMPTYPAREN { Unit }

 | i = NUM  { Int i }
 | v = var { Var v }

 | LPAREN; t=block; RPAREN { t }

 | IF_KW; cond=block; THEN_KW; cl1=block; ELSE_KW; cl2=block
   { If (cond, cl1, cl2) }

 | FUNCTION_KW; id = var; COLON; annot=typ; DBLARROW; body=block
 { Function (id, annot, body) }

 | t1=block; PLUS; t2=block { Plus (t1, t2) }

 | t1=block; MINUS; t2=block { Minus (t1, t2) }

 | t1=block; MULT; t2=block { Mult (t1, t2) }

 | t1=block; LT; t2=block { Lt (t1, t2) }
;


var:
 | x = IDENT { x }


typ:
 | UNIT_T_KW { UnitT }
 | INT_T_KW  { IntT }
 | BOOL_T_KW  { BoolT }
 | LPAREN; t=typ; RPAREN { t }
 | LPAREN; t1=typ; COMMA; t2=typ; RPAREN { Pair2 (t1, t2) }
 | LPAREN; t1=typ; COMMA; t2=typ; COMMA; t3=typ; RPAREN { Pair3 (t1, t2, t3) }
 | t1=typ; ARROW; t2=typ { Arrow (t1, t2) }
;
