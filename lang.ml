exception EvalError of string
exception TimeoutError
exception StackOverflow of string
exception EqualError

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

type decl =
  | DExcept of ctor
  | DEqn of id * typ
  | DData of id * ctor list (* type D = ctors *)
  | DLet of binding
  | DBlock of bool * blinding list (* let x1 = e1 and ... xn = en | let rec f1 = e1 and f2 = e2 and ... fn = en *)
  | TBlock of decl list
and exp =
  (* const *)
  | EUnit
  | Const of int
  | TRUE
  | FALSE
  | EList of exp list
  | String of id
  | EVar of id
  | ECtor of id * exp list
  | ETuple of exp list
  (* aop *)
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | MOD of exp * exp
  | MINUS of exp
  (* bop *)
  | NOT of exp
  | OR of exp * exp
  | AND of exp * exp
  | LESS of exp * exp
  | LARGER of exp * exp
  | EQUAL of exp * exp
  | NOTEQ of exp * exp
  | LESSEQ of exp * exp
  | LARGEREQ of exp * exp
  (* lop *)
  | AT of exp * exp
  | DOUBLECOLON of exp * exp
  | STRCON of exp * exp
  (* else *)
  | EApp of exp * exp
  | EFun of arg * exp
  | ELet of let_binding * bool * arg list * typ * exp * exp
  | EBlock of bool * binding list * exp
  | EMatch of exp * branch list
  | IF of exp * exp * exp
  | Raise of exp
and branch = pat * exp
and binidng = (let_binding * bool * arg list * typ * exp)

type prog = decl list

type value =
  (* concrete values *)
  | VUnit
  | VInt of int
  | VString of string
  | VBool of bool
  | VList of value list
  | VTuple of value list
  | VCtor of id * value list
  | VFun of arg * exp * env
  | VFunRec of id * arg * exp * env
  | VBlock of id * (id * value) list
  (* symbolic values *)
  | SInt
  | SString
  | SBool
  | SExp
and env = (id, value) BatMap.t
and components = exp BatSet.t

exception EExcept of value

let empty_env = BatMap.empty
let lookup_env = BatMap.find
let update_env = BatMap.add

(* function application *)
let rec appify : exp -> exp list -> exp
= fun exp exp_list ->
  match exp_list with
  | [] -> exp
  | hd::tl -> appify (EApp (exp, hd)) tl

let rec let_to_exp : let_bind -> exp
= fun x ->
  match x with
  | BindOne x -> EVar x
  | BindTuple xs -> ETuple (List.map let_to_exp xs)
  | _ -> raise (Failure "wild-card _ is not valid")