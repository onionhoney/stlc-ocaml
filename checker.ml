open Core

include Stlc_types

exception TypeError of string
exception NormalForm

let rec typecheck env = function
  | Int i -> IntT
  | Unit  -> UnitT
  | Bool b -> BoolT
  | Plus (l, r) -> begin
      match (typecheck env l, typecheck env r) with
      | (IntT, IntT) -> IntT
      | _ -> raise (TypeError "Expecting int + int")
    end
  | Minus (l, r) -> begin
      match (typecheck env l, typecheck env r) with
      | (IntT, IntT) -> IntT
      | _ -> raise (TypeError "Expecting int - int")
    end
  | Mult (l, r) -> begin
      match (typecheck env l, typecheck env r) with
      | (IntT, IntT) -> IntT
      | _ -> raise (TypeError "Expecting int * int")
    end
  | Lt (t1, t2) -> begin
      match (typecheck env t1, typecheck env t2) with
      | (IntT, IntT) -> BoolT
      | _ -> raise (TypeError "Expecting int < int")
    end
  | If (cond, t1, t2) -> begin
      match (typecheck env cond, typecheck env t1, typecheck env t2) with
      | (BoolT, typ1, typ2) when typ1 = typ2 -> typ1
      | (_, typ1, typ2) when typ1 = typ2 -> raise (TypeError "Expecting bool in cond")
      | _ -> raise (TypeError "Expecting same types ")
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
  | Letrec (id, t, v, expr) when isval v -> begin
      let new_env = (id, t) :: env in
      let tv = typecheck new_env v in
      if t = tv then typecheck new_env expr else
        raise (TypeError ("Variable " ^ id ^ " does not have the type it claims"))
    end
  | Letrec (id,_,_,_) -> raise (TypeError (id ^ " in letrec must be a value."))
  | Tuple2 (t1, t2) -> Pair2 (typecheck env t1, typecheck env t2)
  | Tuple3 (t1, t2, t3) -> Pair3 (typecheck env t1, typecheck env t2, typecheck env t3)
