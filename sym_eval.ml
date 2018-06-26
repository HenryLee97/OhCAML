open Lang
open Sym_lang

let rec cartesian : 'a list -> 'b list -> ('a * 'b) list
= fun l1 l2 ->
  match l1 with
  | [] -> []
  | a::tl -> (List.map (fun b -> (a, b)) l2)@(cartesian tl l2)

let rec sym_eval : senv -> path_cond -> exp -> (path_cond * sym_value) list
= fun senv pi e ->
  match e with
  | EUnit -> [(pi, SUnit)]
  | Const n -> [(pi, SInt n)]
  | TRUE -> [(pi, SBool true)]
  | FALSE -> [(pi, SBool false)]
  | String id -> [(pi, SString id)]
  | EVar x -> [(pi, lookup_senv x senv)]
  (*| EList es -> *)
  (*| ETuple es -> *)
  (*| ECtor (c, es) -> *)
  | ADD (e1, e2) ->
    let car = cartesian (sym_eval senv pi e1) (sym_eval senv pi e2) in
    List.map (fun ((p1, v1), (p2, v2)) -> (PPop (OPAND, p1, p2), SAop (OPADD, v1, v2))) car
  | SUB (e1, e2) ->
    let car = cartesian (sym_eval senv pi e1) (sym_eval senv pi e2) in
    List.map (fun ((p1, v1), (p2, v2)) -> (PPop (OPAND, p1, p2), SAop (OPSUB, v1, v2))) car
  | MUL (e1, e2) ->
    let car = cartesian (sym_eval senv pi e1) (sym_eval senv pi e2) in
    List.map (fun ((p1, v1), (p2, v2)) -> (PPop (OPAND, p1, p2), SAop (OPMUL, v1, v2))) car
  | DIV (e1, e2) ->
    let car = cartesian (sym_eval senv pi e1) (sym_eval senv pi e2) in
    List.map (fun ((p1, v1), (p2, v2)) -> (PPop (OPAND, p1, p2), SAop (OPDIV, v1, v2))) car
  | MOD (e1, e2) ->
    let car = cartesian (sym_eval senv pi e1) (sym_eval senv pi e2) in
    List.map (fun ((p1, v1), (p2, v2)) -> (PPop (OPAND, p1, p2), SAop (OPMOD, v1, v2))) car
  | MINUS e -> List.map (fun (p, v) -> (p, SAop (OPSUB, SInt 0, v))) (sym_eval senv pi e)
  | NOT e -> List.map (fun (p, v) -> (p, SBop (OPNOT, v, SBool false)) sym_eval senv pi e
  | OR (e1, e2) ->
    let car = cartesian (sym_eval senv pi e1) (sym_eval senv pi e2) in
    List.map (fun ((p1, v1), (p2, v2)) -> (PPop (OPAND, p1, p2), SBop (OPOR, v1, v2))) car
  | AND (e1, e2) ->
    let car = cartesian (sym_eval senv pi e1) (sym_eval senv pi e2) in
    List.map (fun ((p1, v1), (p2, v2)) -> (PPop (OPAND, p1, p2), SBop (OPAND, v1, v2))) car
  | LESS (e1, e2) ->
    let car = cartesian (sym_eval senv pi e1) (sym_eval senv pi e2) in
    List.map (fun ((p1, v1), (p2, v2)) -> (PPop (OPAND, p1, p2), SBop (OPLESS, v1, v2))) car
  | LARGER (e1, e2) ->
    let car = cartesian (sym_eval senv pi e1) (sym_eval senv pi e2) in
    List.map (fun ((p1, v1), (p2, v2)) -> (PPop (OPAND, p1, p2), SBop (OPGREATER, v1, v2))) car
  | LESSEQ (e1, e2) ->
    let car = cartesian (sym_eval senv pi e1) (sym_eval senv pi e2) in
    List.map (fun ((p1, v1), (p2, v2)) -> (PPop (OPAND, p1, p2), SBop (OPLESSEQ, v1, v2))) car
  | LARGEREQ (e1, e2) ->
    let car = cartesian (sym_eval senv pi e1) (sym_eval senv pi e2) in
    List.map (fun ((p1, v1), (p2, v2)) -> (PPop (OPAND, p1, p2), SBop (OPGREATEREQ, v1, v2))) car
  | EQUAL (e1, e2) ->
    let car = cartesian (sym_eval senv pi e1) (sym_eval senv pi e2) in
    List.map (fun ((p1, v1), (p2, v2)) -> (PPop (OPAND, p1, p2), SBop (OPEQUAL, v1, v2))) car
  | NOTEQ (e1, e2) ->
    let car = cartesian (sym_eval senv pi e1) (sym_eval senv pi e2) in
    List.map (fun ((p1, v1), (p2, v2)) -> (PPop (OPAND, p1, p2), SBop (OPNOTEQUAL, v1, v2))) car
  (* AT (e1, e2) -> *)
  (* DOUBLECOLON (e1, e2) -> *)
  (* STRCON (e1, e2) -> *)
  (* IF (e1, e2, e3) -> *)
  (* ELet (f, is_rec, args, typ, e1, e2) -> *)
  (* EBlock (is_rec, bindings, e2) -> *)
  (* EMatch (e, bs) -> *)
  (* EFun (arg, e) -> *)
  (* EApp (e1, e2) -> *)
  (* Raise e -> *)