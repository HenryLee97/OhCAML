open Lang

type op =
  | OPADD
  | OPSUB
  | OPMUL
  | OPDIV
  | OPMOD
  | OPNOT
  | OPOR
  | OPAND
  | OPLESS
  | OPGREATER
  | OPLESSEQ
  | OPGREATEREQ
  | OPEQAUL
  | OPNOTEQ

type sym_value =
  (* base *)
  | SUnit
  | SInt of int
  | SBool of bool
  | SString of id
  | SList of sym_value list
  | STuplle of sym_value list
  | SCtor of id * sym_value list
  | SFun of arg * exp * env
  | SFunRec of id * arg * exp * env
  | VBlock of id * (id * sym_value) list
  (* symbol *)
  | Symbol of typ * int
  | SVop of op * sym_value * sym_value
and senv = (id, sym_value) BatMap.t
and componets = exp BatSet.t

type path_cond =
  | True | False
  | PVop of op * sym_value * sym_value
  | PPop of op * path_cond * path_cond

  let empty_env = BatMap.empty
  let lookup_senv = BatMap.find
  let update_senv = BatMap.add
