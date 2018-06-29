type id = string

type typ =
  | TUnit
  | TInt
  | TBool
  | TString
  | TBase of id (* user defined *)
  | TList of typ
  | TTuple of typ list
  | TCtor of typ * typ list
  | TArr of typ * typ (* function declaration *)
  | TVar of id (* type variable *)
  | TExn

type ctor = id * typ list

type pat =
  | PUnit
  | PInt of int
  | PBool of bool
  | PVar of id
  | PList of pat list
  | PTuple of pat list
  | PCtor of id * pat list
  | PCons of pat lsit
  | PUnder
  | Pats of pat list

type let_bind =
  | BindUnder
  | BlindOne of id
  | BindTuple of let_bind list

type arg =
  | ArgUnder of typ
  | ArgOne of id * typ
  | ArgTuple of arg list
