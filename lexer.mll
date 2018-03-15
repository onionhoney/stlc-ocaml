{
  open Lexing
  open Stlc
  exception SyntaxError of string
  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }

  let keyword_table = Hashtbl.create 72
  let _ =
  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ ("function", FUNCTION_KW);
      ("fun", FUNCTION_KW);
      ("Unit", UNIT_T_KW);
      ("Int", INT_T_KW);
      ("let", LET_KW);
      ("in", IN_KW)
    ]
}

let digit = ['0'-'9']
let white = [' ' '\t' ]+
let newline = '\n'
let colon = ':'
let id = ['a'-'z' '_' 'A'-'Z']['a'-'z' '_' 'A'-'Z' '0'-'9']*
let arrow = "->"
let dblarrow = "=>"
let emptyparen = '(' white ')'


rule read = parse
  | digit+ as num { NUM (int_of_string num) }
  | emptyparen { EMPTYPAREN }
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | '+'  { PLUS }
  | '*'  { MULT }
  | '='  { EQUAL }
  | id as id {
      try Hashtbl.find keyword_table id with
      | Not_found -> IDENT id
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
