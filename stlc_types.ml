type typ =
  | UnitT
  | IntT
  | BoolT
  | Arrow of typ * typ
  | Pair2 of typ * typ
  | Pair3 of typ * typ * typ

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
  | Tuple2 of value * value
  | Tuple3 of value * value * value
  | Fst of value
  | Snd of value
  | Fst3 of value
  | Snd3 of value
  | Trd3 of value

let rec isval = function
  | Unit | Int _ | Bool _ | Function _ -> true
  | Tuple2 (t1, t2) -> (isval t1) && (isval t2)
  | Tuple3 (t1, t2, t3) -> (isval t1) && (isval t2) && (isval t3)
  | _ -> false

let rec isnonfuncval = function
  | Unit | Int _ | Bool _ -> true
  | Tuple2 (t1, t2) -> (isnonfuncval t1) && (isnonfuncval t2)
  | Tuple3 (t1, t2, t3) -> (isnonfuncval t1) && (isnonfuncval t2) && (isnonfuncval t3)
  | _ -> false

