open Util

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
  | TBase of id  (* user defined*)
  | TList of typ
  | TTuple of typ list
  | TCtor of typ * typ list (*  tbase x , tl *)
  | TArr of typ * typ (*fun t1->t2->t3...*)
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
  | PCons of pat list
  (*| PCons of pat * pat*)
  | PUnder 
  | Pats of pat list

type let_bind =
  | BindUnder (* let _ = ... in x *)
  | BindOne of id (* let x = ... in x *)
  | BindTuple of let_bind list (* let x,y = (..., ...) in x,y *)

type arg = 
  | ArgUnder of typ
  | ArgOne of id * typ
  | ArgTuple of arg list

type decl =
  | DExcept of ctor                                 (* exception x of t *)
  | DEqn of id * typ 
  | DData of id * ctor list                         (* 'type' D = ctors *)
  | DLet  of binding       (* let x [rec] (x1:t1) .. (xn:tn) : t = e *)
  (* and block *)
  | DBlock of bool * binding list (* let x1 = e1 and x2 = e2 ... xn = en | let rec f1 x1 = e1 and f2 x2 = e2 and ... fn xn = en *)
  | TBlock of decl list
and exp =
  (* Const *)
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
  | ADD of exp * exp                                (*a1 + a2*)
  | SUB of exp * exp                                (*a1 - a2*)
  | MUL of exp * exp                                (*a1 * a2*)
  | DIV of exp * exp                                (*a1 / a2*)
  | MOD of exp * exp                                (*a1 % a2*)
  | MINUS of exp
  (* bop *)
  | NOT of exp                                      (*not b1*)
  | OR of exp * exp                                 (*b1 || b2*)
  | AND of exp * exp                                (*b1 && b2*)
  | LESS of exp * exp                               (*a1 < a2*)
  | LARGER of exp * exp                             (*a1 > a2*)
  | EQUAL of exp * exp                              (*a1 == a2*)
  | NOTEQ of exp * exp                              (*a1 <> a2 or a1 != a2*)
  | LESSEQ of exp * exp                             (*a1 <= a2*)
  | LARGEREQ of exp * exp                           (*a1 >= a2*)
  (* lop *)
  | AT of exp * exp
  | DOUBLECOLON of exp * exp
  | STRCON of exp * exp
  (* else *)
  | EApp of exp * exp                               (* e1 e2 *)
  | EFun of arg * exp                               (* fun (x:t1) -> e *)
  | ELet of let_bind * bool * arg list * typ * exp * exp  (* let [rec] (x1:t1) .. (xn:tn) : t = e1 in e2 *)
  | EBlock of bool * binding list * exp (* let x1 = e1 and x2 = e2 and ... xn = en in e' | let rec f1 x1 = e1 and f2 x2 = e2 ... fn xn = en in e' *)
  | EMatch of exp * branch list                     (* match e with bs *)
  | IF of exp * exp * exp
  (*List operation*)
  | Raise of exp
and branch = pat * exp   
and binding = (let_bind * bool * arg list * typ * exp) (* f [rec] x1,x2 :t = e => must divide LET & LETREC later *)

type prog = decl list

(* values *)
type value =
  | VUnit
  | VInt of int
  | VString of string
  | VBool of bool
  | VList of value list (* ?? *)
  | VTuple of value list
  | VCtor of id * value list
  | VFun  of arg * exp * env
  | VFunRec of id * arg * exp * env
  | VBlock of id * (id * value) list
and env = (id, value) BatMap.t
and components = exp BatSet.t

exception EExcept of value

type input = exp list
type example = (input * value)
type examples = (input * value) list

let empty_env = BatMap.empty
let lookup_env = BatMap.find
let update_env = BatMap.add

(* generate a fresh type variable *)
let tvar_num = ref 0
let fresh_tvar () = (tvar_num := !tvar_num + 1; (TVar ("#" ^ string_of_int !tvar_num)))

(* function application *)
let rec appify : exp -> exp list -> exp
= fun exp exp_list ->
	match exp_list with
	[] -> exp
	|hd::tl -> appify (EApp(exp,hd)) tl

let rec let_to_exp : let_bind -> exp
= fun x ->
  match x with 
  | BindOne x -> EVar x
  | BindTuple xs -> ETuple (List.map let_to_exp xs)
  | _ -> raise (Failure "Wild-card _ is not valid")
