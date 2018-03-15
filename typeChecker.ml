open Typedlambda


exception TypeError of string

let isval = function
  | `Unit | `Int _ | `Function _ -> true | _ -> false

exception NormalForm

let rec subst var v term =
  if isval term then term else match term with
  | `Var _ as v1 when var = v1 -> v
  | `Plus (x, y) -> `Plus (subst var v x, subst var v y)
  | `Mult (x, y) -> `Mult (subst var v x, subst var v y)
  | `Funcall (v1, v2) -> `Funcall (subst var v v1, subst var v v2)
  | [< Typedlambda.value] -> term
  (* | `Var _ | `Int _ | `Unit | `Function _ | `Let _ -> term *)

let rec step env term =
  if isval term then raise NormalForm else match term with
  | `Plus (`Int v1, `Int v2) -> `Int (v1 + v2)
  | `Plus (`Int _ as v1, t2) -> `Plus (v1, step env t2)
  | `Plus (t1, t2) -> `Plus (step env t1, t2)

  | `Mult (`Int v1, `Int v2) -> `Int (v1 * v2)
  | `Mult (`Int _ as v1, t2) -> `Mult (v1, step env t2)
  | `Mult (t1, t2) -> `Mult (step env t1, t2)

  | `Funcall (`Function (x, _, t), v) when isval v -> subst x v t
  | `Funcall (`Function _ as f, t) -> `Funcall (f, step env t)
  | `Funcall (t1, t2) -> `Funcall(step env t1, t2)
  | `Function _ | `Int _ | `Unit | `Var -> raise NormalForm
  | `Let (id, v, expr) when isval v -> subst id v expr
  | `Let (id, t, expr) -> `Let (id, step env t, expr)


let rec typecheck env = function
  | `Int i -> `IntT
  | `Unit  -> `UnitT
  | `Plus (l, r) -> begin
    match (typecheck env l, typecheck env r) with
    | (`IntT, `IntT) -> `IntT
    | _ -> raise (TypeError "Expecting int + int")
    end
  | `Mult (l, r) -> begin
    match (typecheck env l, typecheck env r) with
    | (`IntT, `IntT) -> `IntT
    | _ -> raise (TypeError "Expecting int * int")
    end
  | `Function (id, t1, expr) -> begin
    let new_env = (id, t1) :: env in
    match (typecheck new_env expr) with
    | t2 -> `Arrow (t1, t2)
    end
  | `Funcall (t1, t2) -> begin
    match (typecheck env t1, typecheck env t2) with
    | (`Arrow (t11, t12), t2) when t11 = t2 -> t12
    | _ -> raise (TypeError "Wrong funcall type")
    end
  | `Var x -> begin
      try List.assoc x env with Not_found ->
        raise (TypeError ("Variable " ^ x ^ "Not found"))
    end
  | `Let (id, v, expr) -> begin
      let tv = typecheck env v in
      let new_env = (id, tv) :: env in
      let texpr = typecheck new_env expr in texpr
     end



