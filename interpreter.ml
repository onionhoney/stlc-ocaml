open Core

include Stlc_types

exception TypeError of string
exception NormalForm
exception SubstError

let rec subst var v term =
  let cont = subst var v in
  (* Printf.printf "substing %s to " var;
   * output_value stdout v;
   * Printf.printf "   in term:";
   * output_value stdout term;
   * Printf.printf "\n"; *)
  if isnonfuncval term then term else match term with
  | Var v1 when var = v1 -> v
  | Var _ -> term
  | Plus (t1, t2) -> Plus (cont t1, cont t2)
  | Minus (t1, t2) -> Minus (cont t1, cont t2)
  | Mult (t1, t2) -> Mult (cont t1, cont t2)
  | Funcall (v1, v2) -> Funcall (cont v1, cont v2)
  | If (cond, t1, t2) -> If (cont cond, cont t1, cont t2)
  | Lt (t1, t2) -> Lt (cont t1, cont t2)
  | Function (t1, typ, t) ->
      (* Printf.printf "trying to replace %s in a fun of %s" var t1; *)
      let newt = if t1 <> var then cont t else t in
      Function (t1, typ, newt)
  | Letrec (id, _, _, _) when id = var -> term
  | Letrec (id, typ, v1, expr) -> Letrec (id, typ, cont v1, cont expr)
  | Let (id, t, expr) -> Let (id, cont t, cont expr)
  | Tuple2 (t1, t2) -> Tuple2 (cont t1, cont t2)
  | Tuple3 (t1, t2, t3) -> Tuple3 (cont t1, cont t2, cont t3)
  | _ ->
    Printf.eprintf "Do not know how to substitute : %a \n" Printer.output_value term;
    Printf.eprintf "In : var = %s, v = %a" var Printer.output_value v;
    raise SubstError

  (* | Var _ | Int _ | Unit | Function _ | Let _ -> term *)

let rec step env term =
  if isval term then raise NormalForm else match term with
  | Plus (Int v1, Int v2) -> Int (v1 + v2)
  | Plus (Int _ as v1, t2) -> Plus (v1, step env t2)
  | Plus (t1, t2) -> Plus (step env t1, t2)

  | Minus (Int v1, Int v2) -> Int (v1 - v2)
  | Minus (Int _ as v1, t2) -> Minus (v1, step env t2)
  | Minus (t1, t2) -> Minus (step env t1, t2)

  | Mult (Int v1, Int v2) -> Int (v1 * v2)
  | Mult (Int _ as v1, t2) -> Mult (v1, step env t2)
  | Mult (t1, t2) -> Mult (step env t1, t2)

  | If (Bool b, t1, t2) -> if b then t1 else t2
  | If (cond, t1, t2) -> If (step env cond, t1, t2)

  | Lt (Int v1, Int v2) -> Bool (v1 < v2)
  | Lt (Int _ as v1, t2) -> Lt (v1, step env t2)
  | Lt (t1, t2) -> Lt (step env t1, t2)

  | Funcall (Function (t1, _, t), v) when isval v -> subst t1 v t
  | Funcall (Function _ as f, t) -> Funcall (f, step env t)
  | Funcall (t1, t2) -> Funcall(step env t1, t2)

  | Let (id, v, expr) when isval v -> subst id v expr
  | Let (id, t, expr) -> Let (id, step env t, expr)

  | Letrec (id, t, v, expr) ->
    let newt = Letrec (id, t, v, Var id) in
    let newv = subst id newt v in
    Let (id, newv, expr)

  | Tuple2 (t1, t2) when not (isval t1) -> Tuple2 (step env t1, t2)
  | Tuple2 (t1, t2) when not (isval t2) -> Tuple2 (t1, step env t2)
  | Tuple3 (t1, t2, t3) when not (isval t1) -> Tuple3 (step env t1, t2, t3)
  | Tuple3 (t1, t2, t3) when not (isval t2) -> Tuple3 (t1, step env t2, t3)
  | Tuple3 (t1, t2, t3) when not (isval t3) -> Tuple3 (t1, t2, step env t3)

  | Var _ ->
    Printf.eprintf "Stuck at var: %a\n" Printer.output_value term;
    raise NormalForm
  | _ ->
    Printf.eprintf "reached impossible state when stepping: %a\n" Printer.output_value term;
    raise NormalForm  (* When isval *)
