open Core.Std
open Cbvterm

module IntAst = Intlib.Ast
module Ident = Intlib.Ident
module Basetype = Intlib.Basetype
module Type = Intlib.Type

(** The term \star:I *)
let star = 
  let open IntAst in
  let z = Ident.fresh "z" in
  mkFn(PatVar z, mkReturn (mkVar z))
    
(** Some syntax to make Int-terms easier to write. *)
let (!!) x = IntAst.mkVar x
let ( ++ ) p1 p2 = IntAst.mkPairV p1 p2
let ( +@ ) = IntAst.mkApp
let ( =>) k t = IntAst.mkFun((k, Basetype.newvar(), Type.newvar()), t)
let mkLet x e1 e2 = IntAst.mkBind e1 (x, e2)
let mkPair x y = IntAst.mkTerm (IntAst.Pair(x, y))
let mkLetPair x y = IntAst.mkTerm (IntAst.LetPair (x, y))

(** Some syntax to make Int-patterns easier to write. *)
let (!?) x = IntAst.PatVar x
let ( +? ) p1 p2 = IntAst.PatPair(p1, p2)

(** Sequential composition of a list of functions. 

    Uses a constant cmp: ('e -> ['f]) -> ('f -> ['d]) -> 'e -> ['d],
    which is defined in prelude.int and which is more stack efficient than 
    \f -> \g -> fn x -> let y = f(x) in g(y),
    as x is thrown away as soon as f(x) returns a result.
*)
let rec compose ts =
  let cmp = Ident.global "cmp" in
  match ts with
  | [] -> assert false
  | [t] -> t
  | t::ts -> !!cmp +@ t +@ (compose ts)
            
(** Constant fix: (X -> X) -> X
    See prelude.int for the definition. 
*)
let fix = Ident.global "fix"

(** Constant cse: ('p1 -> ['n1]) -> ('o1 -> ['n1]) -> 'p1 + 'o1 -> ['n1]
    (case distinction)
    See prelude.int for the definition. 
*)
let cse = Ident.global "cse"

(** Construct block-terms of the form
   [fn p ->
         let x1 = t1 in
         ...
         let xn = tn in
         return v]
   using the call [block p [(x1, t1);...; (xn, tn)] v].
*)
let block p lets v =
  let open IntAst in
  mkFn(p, List.fold_right lets
            ~f:(fun (x, t) r -> mkLet (PatVar x) t r)
            ~init:(mkReturn v))

(** Turn a term of tyoe [ 'a -> ['b] ] into one of type [ 'c * 'a -> ['c * 'b] ].
*)
let liftl t =
  let open IntAst in
  let alpha = Basetype.newvar() in
  (* alternative variant with full control over stack information: *)
  (*
  let encode1 x = mkConst(Cencode(alpha)) +@ x in
  let decode1 x = mkConst(Cdecode(alpha)) +@ x in
  *)
  let encode1 x = mkConst(Cpush(alpha)) +@ x in
  let decode1 x = mkConst(Cpop(alpha)) +@ mkUnitV in
  let x = Ident.fresh "x" in
  let y = Ident.fresh "y" in
  let g = Ident.fresh "g" in
  let e = Ident.fresh "e" in
  compose [mkFn(!?g +? !?x,
                mkLet !?e (encode1 !!g)
                  (mkReturn (!!e ++ !!x)));
           t;
           mkFn(!?e +? !?y,
                mkLet !?g (decode1 !!e)
                  (mkReturn (!!g ++ !!y)))]
                     
(** Interactive variant of variable [x]. *)
let ivar x = Ident.global ("i" ^ Ident.to_string x)
               
(** Value variant of variable [x]. *)
let vvar x = Ident.global ("v" ^ Ident.to_string x)

(** Parameterised monad [M X = X \times exp(A,B)] *)
let monad x a b =
  let open Basetype in
  let expab = Type.newty (Type.FunV(a, Type.newty (Type.Base b))) in
  Type.newty  (Type.Tensor(x, expab)) 

(** Translation of types; quantification is expanded manually using
    explict encode and decode functions. *)
let rec translate_type a =
  let open Basetype in
  match Cbvtype.case a with
  | Cbvtype.Var  -> Type.newvar ()
  | Cbvtype.Sgn s ->
    (match s with
     | Cbvtype.Nat  ->
       let zeroB = newty (ZeroB) in
       Type.newty (Type.FunV(zeroB, Type.newty (Type.Base zeroB)))
     | Cbvtype.Pair(a1, a2) ->
       let ta1 = translate_type a1 in
       let ta2 = translate_type a2 in
       Type.newty (Type.Tensor(ta1, ta2))
     | Cbvtype.Fun(a1, (bin, bout, a2)) ->
       (* monad ([a1] -> monad bin bout [a2]*)
       let ta1 = translate_type a1 in
       let ta2 = translate_type a2 in
       Type.newty (Type.FunI(newvar(), ta1, monad ta2 bin bout)) 
    )

(** Translation of terms *)
let translate (t: Cbvterm.t): IntAst.t =
  let g = Ident.fresh "g" in
  let rec translate (t: Cbvterm.t): IntAst.t =
    let rec pattern_of_context phi =
      match phi with
      | [] -> IntAst.PatUnit
      | (x, _) :: xs -> (pattern_of_context xs) +? !?(vvar x) in
    let rec tuple_of_context phi =
      match phi with
      | [] -> IntAst.mkUnitV
      | (x, _) :: xs -> (tuple_of_context xs) ++ !!(vvar x) in
    match t.t_desc with
    | Var x -> 
      let open IntAst in
      let access = mkVar (ivar x) in
      let code = block
                   (!?g +? (pattern_of_context t.t_context))
                   []
                   (!!g ++ !!(vvar x)) in
      mkPair access code
    | Const Ast.Cintadd ->
      let open IntAst in
      let x = Ident.fresh "x" in
      let ctx = Ident.fresh "ctx" in
      let m = Ident.fresh "m" in
      let n = Ident.fresh "n" in
      let sum = Ident.fresh "sum" in
      let ts = mkPair
                 star
                 (block
                    (!?g +? ((!?ctx +? !?m) +? !?n))
                    [sum, mkConst(Cintadd) +@ (!!m ++ !!n)]
                    (!!g ++ !!sum)) in
      let absn = mkPair
                   (x => ts)
                   (block
                      (!?g +? (PatUnit +? !?m))
                      []
                      (!!g ++ (mkUnitV ++ !!m))) in
      let absnm = mkPair
                    (x => absn)
                    (mkFn(!?g +? !?ctx, mkReturn (!!g ++ mkUnitV))) in
      absnm
    | Const Ast.Cfix ->
      let open IntAst in
      let a = Ident.fresh "a" in
      let r = Ident.fresh "r" in
      let f = Ident.fresh "f" in
      let vf = Ident.fresh "vf" in
      let va = Ident.fresh "a" in
      let vh = Ident.fresh "h" in
      let ctx = Ident.fresh "ctx" in
      let a1 = Ident.fresh "a1" in
      let a2 = Ident.fresh "a2" in
      let b1 = Ident.fresh "b1" in
      let b2 = Ident.fresh "b2" in
      let step f =
        r (*: (A->TB) *) =>
        (a (*: A *) =>
         mkLetPair (f +@ !!r)
           (a1, a2, mkLetPair (!!a1 +@ !!a)
                      (b1, b2, mkPair !!b1
                                 (compose
                                    [block
                                       (!?g +? (!?vf +? !?va))
                                       []
                                       ((!!g ++ !!va) ++ (!!vf ++ !!vf));
                                     liftl !!a2;
                                     block 
                                       ((!?g +? !?va) +? !?vh)
                                       []
                                       (!!g ++ (!!vh ++ !!va));
                                     liftl !!b2])))) in
      mkPair
        (f => mkPair
                (!!fix +@ (step !!f))
                (mkFn(!?g +? (PatUnit +? !?vf),
                      mkReturn (!!g ++ !!vf))))
        (mkFn(!?g +? !?ctx, mkReturn (!!g ++ mkUnitV)))
    | Const Ast.Cintprint ->
      let open IntAst in
      let x = Ident.fresh "x" in
      let ctx = Ident.fresh "ctx" in
      let n = Ident.fresh "n" in
      let ts = mkPair
                 star
                 (mkFn(!?g +? (!?ctx +? !?n),
                       mkLet (PatUnit) (mkConst(Cintprint) +@ !!n)
                         (mkReturn (!!g ++ !!n)))) in
      let absn = mkPair
                   (x => ts)
                   (mkFn(!?g +? !?ctx,
                         mkReturn (!!g ++ mkUnitV))) in
      absn
    | Num i ->
      let open IntAst in
      mkPair
        star
        (mkFn(!?g +? pattern_of_context t.t_context,
              mkReturn (!!g ++ (mkConstV(Cintconst i)))))
    | Fun ((xs, _), s) ->
      let open IntAst in
      let ts = translate s in
      let x = Ident.fresh "x" in
      let e1 = Ident.fresh "e1" in
      let a1 = Ident.fresh "fun1" in
      let a2 = Ident.fresh "fun2" in
      let ctx = Ident.fresh "ctx" in
      let alpha = Basetype.newvar() in
      let encode1 x = mkConst(Cencode(alpha)) +@ x in
      let decode1 x = mkConst(Cdecode(alpha)) +@ x in
      let phi = List.map (s.t_context)
                  ~f:(fun (y, a) -> if List.mem xs y then (x, a) else (y, a)) in
      let t1 = ((ivar x) =>
                let t2 =
                  mkLetPair ts
                    (a1, a2, mkPair
                               !!a1
                               (compose [mkFn(!?g +? (!?e1 +? !?(vvar x)),
                                              mkLet (pattern_of_context t.t_context) (decode1 !!e1)
                                                (mkReturn (!!g ++ (tuple_of_context phi))));
                                         !!a2])) in
                if xs <> [] then
                  mkCopy !!(ivar x) (List.map ~f:ivar xs, t2)
                else t2) in
      mkPair
        t1
        (mkFn(!?g +? !?ctx,
              mkLet !?e1 (encode1 !!ctx) (
                mkReturn (!!g ++ !!e1))))
    | App(s1, s2) ->
      let open IntAst in
      let ts1 = translate s1 in
      let ts2 = translate s2 in
      let vf = Ident.fresh "f" in
      let vx = Ident.fresh "x" in
      let ctx2 = Ident.fresh "ctx2" in
      let s1a = Ident.fresh "s1a" in
      let s1c = Ident.fresh "s2c" in
      let s2a = Ident.fresh "s2a" in
      let s2c = Ident.fresh "s2c" in
      let a1 = Ident.fresh "app1" in
      let a2 = Ident.fresh "app2" in
      let app =
        mkLetPair (!!s1a +@ !!s2a)
          (a1, a2, mkPair
                     !!a1
                     (compose [mkFn(!?g +? (pattern_of_context t.t_context),
                                    mkReturn ((!!g ++ (tuple_of_context s2.t_context)) ++ (tuple_of_context s1.t_context)));
                               liftl !!s1c;
                               mkFn((!?g +? !?ctx2) +? !?vf,
                                    mkReturn ((!!g ++ !!vf) ++ !!ctx2));
                               liftl !!s2c;
                               mkFn((!?g +? !?vf) +? !?vx,
                                    mkReturn (!!g ++ (!!vf ++ !!vx)));
                               !!a2])) in
      mkTerm (LetPair (ts1, (s1a, s1c, mkTerm (LetPair (ts2, (s2a, s2c, app))))))
    | Pair(t1, t2) ->
      let open IntAst in
      let tt1 = translate t1 in
      let tt2 = translate t2 in
      let t1a = Ident.fresh "a" in
      let t1c = Ident.fresh "c" in
      let t2a = Ident.fresh "a" in
      let t2c = Ident.fresh "c" in
      let ctx = Ident.fresh "ctx" in
      let vx1 = Ident.fresh "vx" in
      let vx2 = Ident.fresh "vx" in
      mkLetPair tt1
        (t1a, t1c, mkLetPair tt2
                     (t2a, t2c, mkPair
                                  (mkPair !!t1a !!t2a)
                                  (compose [
                                     mkFn((!?g +? (pattern_of_context t.t_context)),
                                          mkReturn ((!!g ++ (tuple_of_context t2.t_context)) ++ (tuple_of_context t1.t_context)));
                                     liftl !!t1c;
                                     mkFn((!?g +? !?ctx) +? !?vx1,
                                          mkReturn ((!!g ++ !!vx1) ++ !!ctx));
                                     liftl !!t2c;
                                     mkFn((!?g +? !?vx1) +? !?vx2,
                                          mkReturn (!!g ++ (!!vx1 ++ !!vx2)))])))
    | Fst(t1) ->
      let open IntAst in
      let tt1 = translate t1 in
      let t1a = Ident.fresh "a" in
      let t1c = Ident.fresh "c" in
      let a1 = Ident.fresh "a" in
      let a2 = Ident.fresh "c" in
      let vx1 = Ident.fresh "vx" in
      let vx2 = Ident.fresh "vx" in
      mkLetPair tt1
        (t1a, t1c, mkPair
                     (mkLetPair !!t1a (a1, a2, !!a1))
                     (compose [!!t1c;
                               mkFn((!?g +? (!?vx1 +? !?vx2),
                                     mkReturn (!!g ++ !!vx1)))]))
    | Snd(t1) ->
      let open IntAst in
      let tt1 = translate t1 in
      let t1a = Ident.fresh "a" in
      let t1c = Ident.fresh "c" in
      let a1 = Ident.fresh "a" in
      let a2 = Ident.fresh "c" in
      let vx1 = Ident.fresh "vx" in
      let vx2 = Ident.fresh "vx" in
      mkLetPair tt1
        (t1a, t1c, mkPair
                     (mkLetPair !!t1a (a1, a2, !!a2))
                     (compose [!!t1c;
                               mkFn((!?g +? (!?vx1 +? !?vx2),
                                     mkReturn (!!g ++ !!vx2)))]))
    | Ifz(s, tt, tf) -> 
      let open IntAst in
      let ts = translate s in
      let ttt = translate tt in
      let ttf = translate tf in
      let ctxt = Ident.fresh "ctxt" in
      let ctxf = Ident.fresh "ctxf" in
      let tsc = Ident.fresh "k" in
      let tttc = Ident.fresh "k1" in
      let ttfc = Ident.fresh "k2" in
      let k = Ident.fresh "k" in
      let vs = Ident.fresh "s" in
      let eq = Ident.fresh "eq" in
      let ifz =
        mkPair
          star
          (compose [mkFn((!?g +? (pattern_of_context t.t_context)),
                         mkReturn ((!!g ++ ((tuple_of_context tt.t_context) ++ (tuple_of_context tf.t_context))) ++
                                   (tuple_of_context s.t_context)));
                    liftl !!tsc;
                    mkFn((!?g +? (!?ctxt +? !?ctxf)) +? !?vs,
                         (mkLet !?eq (mkConst(Cinteq) +@ (!!vs ++ (mkConstV(Cintconst 0))))
                            (mkCase (Basetype.Data.boolid) !!eq
                               [(PatUnit, mkReturn (mkInlV (!!g ++ !!ctxt)));
                                (PatUnit, mkReturn (mkInrV (!!g ++ !!ctxf)))])));
                    !!cse +@ !!tttc +@ !!ttfc]) in
      mkLetPair ts
        (k, tsc, mkLetPair ttt
                   (k, tttc, mkLetPair ttf
                               (k, ttfc, ifz)))
  in
  IntAst.mkTypeAnnot (translate t)
    (monad (translate_type (t.t_type)) (Basetype.newvar()) (Basetype.newvar()))
