type typ = [
  | `UnitT
  | `IntT
  | `Arrow of typ * typ
]

type value = [
  | `Unit
  | `Int of int
  | `Var of string
  | `Plus of value * value
  | `Mult of value * value
  | `Function of string * typ * value
  | `Funcall of value * value
  | `Let of string * value * value
]


open Core
let rec output_value outc = function
  | `Unit  -> Out_channel.output_string outc "'()"
  | `Int i -> fprintf outc "%d" i
  | `Var x -> fprintf outc "%s" x
  | `Plus (l, r) -> print_arith "+" outc l r
  | `Mult (l, r) -> print_arith "*" outc l r
  | `Function (s, t, v) -> print_function outc s t v
  | `Funcall (v1, v2) -> print_funcall outc v1 v2
  | `Let (id, v, e) -> print_let outc id v e

and print_arith sign outc l r =
  Out_channel.output_string outc ("(" ^ sign ^ " ");
  output_value outc l;
  Out_channel.output_string outc " ";
  output_value outc r;
  Out_channel.output_string outc ")"

and print_function outc id typ expr =
  fprintf outc "(Function %s " id;
  print_type outc typ;
  Out_channel.output_string outc " ";
  output_value outc expr;
  Out_channel.output_string outc ")";

and print_let outc id v e =
  Out_channel.output_string outc ("(Let " ^ id ^ " = ");
  output_value outc v;
  Out_channel.output_string outc " in \n";
  output_value outc e;
  Out_channel.output_string outc ")"

and print_funcall outc v1 v2 =
  fprintf outc "(Funcall ";
  output_value outc v1;
  Out_channel.output_string outc " ";
  output_value outc v2;
  Out_channel.output_string outc ")";

and print_type outc = function
  | `UnitT -> Out_channel.output_string outc "Unit"
  | `IntT -> Out_channel.output_string outc "Int"
  | `Arrow (l, r) ->
    Out_channel.output_string outc "(";
    print_type outc l;
    Out_channel.output_string outc " -> ";
    print_type outc r;
    Out_channel.output_string outc ")";


