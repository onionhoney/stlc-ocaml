type typ =
  | UnitT
  | IntT
  | Arrow of typ * typ

type var = string
type value =
  | Unit
  | Int of int
  | Var of var
  | Plus of value * value
  | Mult of value * value
  | Function of var * typ * value
  | Funcall of value * value
  | Let of var * value * value


open Core
let rec output_value outc = function
  | Unit  -> Out_channel.output_string outc "'()"
  | Int i -> fprintf outc "%d" i
  | Var x -> fprintf outc "%s" x
  | Plus (l, r) -> print_arith "+" outc l r
  | Mult (l, r) -> print_arith "*" outc l r
  | Function (s, t, v) -> print_function outc s t v
  | Funcall (v1, v2) -> print_funcall outc v1 v2
  | Let (id, v, e) -> print_let outc id v e

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
  Out_channel.output_string outc "⊂\n";
  output_value outc e;
  Out_channel.output_string outc ""

and print_funcall outc v1 v2 =
  fprintf outc "(";
  output_value outc v1;
  Out_channel.output_string outc " ";
  output_value outc v2;
  Out_channel.output_string outc ")";

and print_type outc = function
  | UnitT -> Out_channel.output_string outc "Unit"
  | IntT -> Out_channel.output_string outc "Int"
  | Arrow (l, r) ->
    Out_channel.output_string outc "(";
    print_type outc l;
    Out_channel.output_string outc " -> ";
    print_type outc r;
    Out_channel.output_string outc ")";




exception TypeError of string

let isval = function
  | Unit | Int _ | Function _ -> true | _ -> false

let isnotfuncval = function
  | Unit | Int _ | _ -> false

exception NormalForm

let rec subst var v term =
  (* Printf.printf "substing %s to " var;
   * output_value stdout v;
   * Printf.printf "   in term:";
   * output_value stdout term;
   * Printf.printf "\n"; *)
  if isnotfuncval term then term else match term with
  | Var v1 when var = v1 -> v
  | Plus (x, y) -> Plus (subst var v x, subst var v y)
  | Mult (x, y) -> Mult (subst var v x, subst var v y)
  | Funcall (v1, v2) -> Funcall (subst var v v1, subst var v v2)
  | Function (x, typ, t) ->
      (* Printf.printf "trying to replace %s in a fun of %s" var x; *)
      let newt = if x <> var then subst var v t else t in
      Function (x, typ, newt)
  | _ -> term
  (* | Var _ | Int _ | Unit | Function _ | Let _ -> term *)

let rec step env term =
  if isval term then raise NormalForm else match term with
  | Plus (Int v1, Int v2) -> Int (v1 + v2)
  | Plus (Int _ as v1, t2) -> Plus (v1, step env t2)
  | Plus (t1, t2) -> Plus (step env t1, t2)

  | Mult (Int v1, Int v2) -> Int (v1 * v2)
  | Mult (Int _ as v1, t2) -> Mult (v1, step env t2)
  | Mult (t1, t2) -> Mult (step env t1, t2)

  | Funcall (Function (x, _, t), v) when isval v -> subst x v t
  | Funcall (Function _ as f, t) -> Funcall (f, step env t)
  | Funcall (t1, t2) -> Funcall(step env t1, t2)
  | Let (id, v, expr) when isval v -> subst id v expr
  | Let (id, t, expr) -> Let (id, step env t, expr)
  | _ -> raise NormalForm


let rec typecheck env = function
  | Int i -> IntT
  | Unit  -> UnitT
  | Plus (l, r) -> begin
    match (typecheck env l, typecheck env r) with
    | (IntT, IntT) -> IntT
    | _ -> raise (TypeError "Expecting int + int")
    end
  | Mult (l, r) -> begin
    match (typecheck env l, typecheck env r) with
    | (IntT, IntT) -> IntT
    | _ -> raise (TypeError "Expecting int * int")
    end
  | Function (id, t1, expr) -> begin
    let new_env = (id, t1) :: env in
    match (typecheck new_env expr) with
    | t2 -> Arrow (t1, t2)
    end
  | Funcall (t1, t2) -> begin
    match (typecheck env t1, typecheck env t2) with
    | (Arrow (t11, t12), t2) when t11 = t2 -> t12
    | _ -> raise (TypeError "Wrong funcall type")
    end
  | Var x -> begin
      try ListLabels.assoc x env with Not_found ->
        raise (TypeError ("Variable " ^ x ^ "Not found"))
    end
  | Let (id, v, expr) -> begin
      let tv = typecheck env v in
      let new_env = (id, tv) :: env in
      let texpr = typecheck new_env expr in texpr
     end



