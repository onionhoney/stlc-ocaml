open Core
open Stlc_types


(* if move print_tuple in let-and, it loses polymorphism *)
let print_tuple outc f vals =
  fprintf outc "(";

  let rec _print_rest = function
    | [] -> ()
    | first :: rest -> fprintf outc ", ";
                       f first;
                       _print_rest rest
  in match vals with
     | [] -> ()
     | first :: rest -> f first;
                        _print_rest rest;
                        fprintf outc ")"


let rec output_value outc term = match term with
  | Unit  -> Out_channel.output_string outc "()"
  | Int i -> fprintf outc "%d" i
  | Bool b -> Out_channel.output_string outc (if b then "true" else "false")
  | Var x -> fprintf outc "%s" x
  | Plus (l, r) -> print_arith "+" outc l r
  | Minus (l, r) -> print_arith "-" outc l r
  | Mult (l, r) -> print_arith "*" outc l r
  | Lt (l, r) -> print_arith "<" outc l r
  | If (cond, t1, t2) -> print_if outc cond t1 t2
  | Function (s, t, v) -> print_function outc s t v
  | Funcall (v1, v2) -> print_funcall outc v1 v2
  | Let (id, v, e) -> print_let outc id v e
  | Letrec (id, t, v, e) -> print_let outc id v e
  | Tuple2 (v1, v2) -> print_tuple outc (output_value outc) [v1; v2]
  | Tuple3 (v1, v2, v3) -> print_tuple outc (output_value outc) [v1; v2; v3]
  | Fst t | Snd t | Fst3 t | Snd3 t | Trd3 t -> print_tuple_accessor outc term t

and print_tuple_accessor outc term data =
  let name = function
    | Fst _ -> "fst"
    | Snd _ -> "snd"
    | Fst3 _ -> "fst3"
    | Snd3 _ -> "snd3"
    | Trd3 _ -> "trd3"
    | _ -> "Unknown"
  in
  fprintf outc "%s(" (name term);
  output_value outc data;
  fprintf outc ")"

and print_if outc cond t1 t2 =
  Out_channel.output_string outc "if ";
  output_value outc cond;
  Out_channel.output_string outc " then ";
  output_value outc t1;
  Out_channel.output_string outc " else ";
  output_value outc t2;

and print_arith sign outc l r =
  Out_channel.output_string outc ("(" ^ sign ^ " ");
  output_value outc l;
  Out_channel.output_string outc " ";
  output_value outc r;
  Out_channel.output_string outc ")"

and print_function outc id typ expr =
  fprintf outc "(λ%s." id;
  (* print_type outc typ; *)
  (* Out_channel.output_string outc " "; *)
  output_value outc expr;
  Out_channel.output_string outc ")";

and print_let outc id v e =
  Out_channel.output_string outc ("∃ " ^ id ^ " = ");
  output_value outc v;
  Out_channel.output_string outc " in (\n";
  output_value outc e;
  Out_channel.output_string outc ")"

and print_funcall outc v1 v2 =
  fprintf outc "(";
  output_value outc v1;
  Out_channel.output_string outc " ";
  output_value outc v2;
  Out_channel.output_string outc ")";

and print_type outc = function
  | UnitT -> Out_channel.output_string outc "Unit"
  | BoolT -> Out_channel.output_string outc "Bool"
  | IntT -> Out_channel.output_string outc "Int"
  | Arrow (l, r) ->
    Out_channel.output_string outc "(";
    print_type outc l;
    Out_channel.output_string outc " -> ";
    print_type outc r;
    Out_channel.output_string outc ")";
  | Pair2 (tt1, tt2) -> print_tuple outc (print_type outc) [tt1; tt2]
  | Pair3 (tt1, tt2, tt3) -> print_tuple outc (print_type outc) [tt1; tt2; tt3]
