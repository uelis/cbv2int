(** Type inference *)
open Core.Std

module Ident = Intlib.Ident
module Printing = Intlib.Printing
module Basetype = Intlib.Basetype
module Uftype = Intlib.Uftype
                 
(* Contexts *)
type 'a context = (Ident.t * 'a) list

exception Typing_error of Ast.t option * string

let eq_expected_constraint t ~expected:expected_ty ~actual:actual_ty =
  try
    Cbvtype.unify_exn expected_ty actual_ty
  with
  | Uftype.Cyclic_type ->
    let msg = "Unification leads to cyclic type." in
    raise (Typing_error(None, msg)) 
  | Uftype.Constructor_mismatch ->
    let msg = Printf.sprintf
                "Term has interactive type %s, but a term of type %s is expected."
                (Cbvtype.to_string actual_ty)
                (Cbvtype.to_string expected_ty) in
    raise (Typing_error(Some t, msg))

let funty a b =
  Cbvtype.newty
    (Cbvtype.Fun(a, (Basetype.newvar(),
                     Basetype.newvar(), b))) 

let rec pt (phi: Cbvtype.t context) (t: Ast.t)
  : Cbvterm.t * (Ident.t * Ident.t) list =
  let open Cbvterm in
  match t.Ast.desc with
  | Ast.Var(v: Ident.t) ->
    let a =
      match List.Assoc.find phi v with
      | Some a -> a
      | None ->
        let msg = "Variable '" ^ (Ident.to_string v) ^ "' not bound." in
        raise (Typing_error (Some t, msg)) in
    let v' = Ident.variant v in
    { t_desc = Cbvterm.Var(v');
      t_type = a;
      t_context = [(v', a)];
      t_loc = t.Ast.loc},
    [(v, v')]
  | Ast.Const(Ast.Cintprint as c) ->
    let a = funty
              (Cbvtype.newty Cbvtype.Nat)
              (Cbvtype.newty Cbvtype.Nat) in
    { t_desc = Const(c);
      t_type = a;
      t_context = [];
      t_loc = t.Ast.loc},
    []
  | Ast.Const(Ast.Cintadd as c) ->
    let a = funty
              (Cbvtype.newty Cbvtype.Nat)
              (funty              
                 (Cbvtype.newty Cbvtype.Nat)
                 (Cbvtype.newty Cbvtype.Nat)) in 
    { t_desc = Const(c);
      t_type = a;
      t_context = [];
      t_loc = t.Ast.loc},
    []
  | Ast.Const(Ast.Cfix as c) ->
    let alpha = Cbvtype.newvar() in
    let beta = Cbvtype.newvar() in
    let ab = funty alpha beta in
    { t_desc = Const(c);
      t_type = funty (funty ab ab) ab;
      t_context = [];
      t_loc = t.Ast.loc},
    []
  | Ast.App(s, t) ->
    let s1, sinstances = pt phi s in
    let a = Basetype.newvar() in
    let b = Basetype.newvar() in
    let beta = Cbvtype.newvar() in
    let t1, tinstances = pt phi t in
    eq_expected_constraint s
      ~actual:s1.t_type
      ~expected:(Cbvtype.newty (Cbvtype.Fun(t1.t_type, (a, b, beta))));
    { t_desc = App(s1, t1);
      t_type = beta;
      t_context = s1.t_context @ t1.t_context;
      t_loc = t.Ast.loc },
    sinstances @ tinstances
  | Ast.Fun(x, t) ->
    let alpha = Cbvtype.newvar() in
    let a = Basetype.newvar() in
    let b = Basetype.newvar() in
    let phi1 = (x, alpha) :: phi in
    let t1, tinstances = pt phi1 t in
    let xs = List.filter_map tinstances
               ~f:(fun (y, y') -> if x = y then Some y' else None) in
    let instances = List.filter tinstances ~f:(fun (y, _) -> y <> x) in
    { t_desc = Fun((xs, alpha), t1);
      t_type = Cbvtype.newty (Cbvtype.Fun(alpha, (a, b, t1.t_type)));
      t_context = List.filter t1.t_context ~f:(fun (y, _) -> not (List.mem xs y));
      t_loc = t.Ast.loc },
    instances
  | Ast.Pair(s, t) ->
    let s1, sinstances = pt phi s in
    let t1, tinstances = pt phi t in
    { t_desc = Pair(s1, t1);
      t_type = (Cbvtype.newty (Cbvtype.Pair(s1.t_type, t1.t_type)));
      t_context = s1.t_context @ t1.t_context;
      t_loc = t.Ast.loc },
    sinstances @ tinstances
  | Ast.Fst(t) ->
    let t1, tinstances = pt phi t in
    let a = Cbvtype.newvar() in
    let b = Cbvtype.newvar() in
    eq_expected_constraint t
      ~actual:t1.t_type
      ~expected:(Cbvtype.newty (Cbvtype.Pair(a, b)));
    { t_desc = Fst(t1);
      t_type = a;
      t_context = t1.t_context;
      t_loc = t.Ast.loc },
    tinstances
  | Ast.Snd(t) ->
    let t1, tinstances = pt phi t in
    let a = Cbvtype.newvar() in
    let b = Cbvtype.newvar() in
    eq_expected_constraint t
      ~actual:t1.t_type
      ~expected:(Cbvtype.newty (Cbvtype.Pair(a, b)));
    { t_desc = Snd(t1);
      t_type = b;
      t_context = t1.t_context;
      t_loc = t.Ast.loc },
    tinstances
  | Ast.Num(n) ->
    { t_desc = Num(n);
      t_type = Cbvtype.newty Cbvtype.Nat;
      t_context = [];
      t_loc = t.Ast.loc },
    []
  | Ast.Ifz(s, tt, tf) ->
    let sa, sinstances = pt phi s in
    let tta, ttinstances = pt phi tt in
    let tfa, tfinstances = pt phi tf in
    eq_expected_constraint s
      ~actual:sa.t_type
      ~expected:(Cbvtype.newty Cbvtype.Nat);
    eq_expected_constraint tt
      ~actual:tta.t_type
      ~expected:(Cbvtype.newty Cbvtype.Nat);
    eq_expected_constraint tt
      ~actual:tfa.t_type
      ~expected:(Cbvtype.newty Cbvtype.Nat);
    { t_desc = Ifz(sa, tta, tfa);
      t_type = tta.t_type;
      t_context = sa.t_context @ tta.t_context @ tfa.t_context;
      t_loc = t.Ast.loc },
    sinstances @ ttinstances @ tfinstances

let check_term (phi: Cbvtype.t context) (t: Ast.t)
  : Cbvterm.t =
  let a, _ = pt phi t in
  a
