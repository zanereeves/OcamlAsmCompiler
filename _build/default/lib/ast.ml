type op =
  | Neg
  | Not

type bop = 
  | Add
  | Mult
  | Leq
  | LessT
  | Neq
  | GreatT
  | Minus
  | Div
  | Eq


type param = 
  | TParam of string


type expr = 
  | Var of string
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr
  | Uop of op * expr
  | Call of string * expr list
  
type statement = 
  | If of expr * statement * statement
  | Let of string * statement
  | ExprS of expr
  | ReturnS of expr
  | WhileS of expr * statement
  | Main of statement
  | FuncA of string * param list * statement
  | Block of statement list
  | Obj of statement
  | Assert of expr


type program = Prog of statement list