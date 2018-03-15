open Core
open Lexer
open Lexing
open TypeChecker

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Stlc.input Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Stlc.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
    printf "%a\n" Typedlambda.output_value value;
    parse_and_print lexbuf
  | None -> ()

exception TypeError of string
let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

  match parse_with_error lexbuf with
  | Some value -> (
    printf "%a\n" Typedlambda.output_value value;
    try TypeChecker.typecheck [] value |> ignore;
      let steped = TypeChecker.step [] value in
      printf "Stepped: %a\n" Typedlambda.output_value steped;
    printf "Passes Typechecking" with
    | TypeError s ->
      fprintf stderr "%s" ("Failed typchecking. " ^ s ))
  | None -> fprintf stderr "Error in parsing";
  In_channel.close inx

let () =
  Command.basic ~summary:"Parse and display STLC"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop 
  |> Command.run
