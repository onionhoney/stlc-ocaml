{
  open Lexing
  open Parser
  open Stlc_types

  exception SyntaxError of string
  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }

  let keyword_table = Hashtbl.create 72
  let tuple_kw_table = Hashtbl.create 10
  let _ =
  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ ("function", FUNCTION_KW);
      ("fun", FUNCTION_KW);
      ("Unit", UNIT_T_KW);
      ("Int", INT_T_KW);
      ("Bool", BOOL_T_KW);
      ("let", LET_KW);
      ("letrec", LETREC_KW);
      ("in", IN_KW);
      ("if", IF_KW);
      ("then", THEN_KW);
      ("else", ELSE_KW);
      ("true", TRUE_KW);
      ("false", FALSE_KW);
    ]
  let _ =
  List.iter (fun (kwd, tok) -> Hashtbl.add tuple_kw_table kwd tok)
    [ ("fst", fun x -> Fst x);
      ("snd", fun x -> Snd x);
      ("fst3", fun x -> Fst3 x);
      ("snd3", fun x -> Snd3 x);
      ("trd",  fun x -> Trd3 x);
      ("trd3", fun x -> Trd3 x);
    ]
}

let digit = ['0'-'9']
let white = [' ' '\t' ]+
let newline = '\n'
let colon = ':'
let id = ['a'-'z' '_' 'A'-'Z']['a'-'z' '_' 'A'-'Z' '0'-'9']*
let arrow = "->"
let dblarrow = "=>"
let emptyparen = "()"


rule read = parse
  | digit+ as num { NUM (int_of_string num) }
  | emptyparen { EMPTYPAREN }
  | ','  { COMMA }
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | '+'  { PLUS }
  | '-'  { MINUS }
  | '*'  { MULT }
  | '='  { EQUAL }
  | '<'  { LT }
  | id as id {
      let ans = try Hashtbl.find keyword_table id with | Not_found -> IDENT id
      in
      let ans = try TUPLE_KWS (Hashtbl.find tuple_kw_table id) with | Not_found -> ans
      in ans
    }
  | arrow { ARROW }
  | dblarrow { DBLARROW }
  | colon {COLON}
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | _ { raise (SyntaxError ("Unexpected char: " ^
                            let s = Lexing.lexeme lexbuf in s
                            )) }
  (* | _    { token lexbuf } *)
  | eof  { EOF }

(* raise SyntaxError "String is not terminated"} *)
