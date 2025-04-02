exception DimensionError of string 
exception TypeMismatch of string
exception EvalError of string

type typ =
  | B 
  | Int
  | Float 
  | V of int  
  | M of int * int

type binop =
  | Add | Sub | Mul | Div | Mod
  | And | Or
  | Lt | Gt | Lte | Gte | Eq
  | Dot | Angle

type unop =
   Neg | Not | Magnitude | Dimension | Transpose | Determinant | Inverse
type type_specifier=
   TInt| TFloat |TBool |TV |TM

type expr =
  | ConstBool of bool 
  | ConstInt of int 
  | ConstFloat of float 
  | ConstV of expr list
  | ConstM of expr list list
  | Var of string
  | BinOp of expr * binop * expr
  | UnOp of unop * expr
  | Assign of string * expr
  | Input of type_specifier* string option
  | Index of expr * expr list
  | AssignIdx of expr*expr

type statement =
  | Expr of expr
  | Block of statement list
  | If of expr * statement * statement option
  | While of expr * statement
  | For of string * expr * expr * statement
  | Print of expr
  | Break
  | Continue
  | Vector of int * string * expr
  | Matrix of int * int * string * expr

type program = statement list
