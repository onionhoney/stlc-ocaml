type typ =
  | UnitT
  | IntT
  | BoolT
  | Arrow of typ * typ

type var = string
type value =
  | Unit
  | Int of int
  | Bool of bool
  | Var of var
  | Plus of value * value
  | Minus of value * value
  | Mult of value * value
  | Lt of value * value
  | If of value * value * value
  | Function of var * typ * value
  | Funcall of value * value
  | Let of var * value * value
  | Letrec of var * typ * value * value

let isval = function
  | Unit | Int _ | Bool _ | Function _ -> true | _ -> false

let isnonfuncval = function
  | Unit | Int _ | Bool _ -> true | _ -> false


