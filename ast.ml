open Core.Std
open Intlib
(** Term representation *)

(*  I've read the implementation of John Harrion's HOL light
    and the sources of the OCaml compiler when writing this file. *)

module Location = struct
  type pos = { column: int; line: int}
  type loc = {start_pos: pos; end_pos: pos}
  type t = loc option
  let none = None
end

type const =
  | Cintadd
  | Cintprint
  | Cfix

type t = {
  desc: t_desc;
  loc: Location.t
}
and t_desc =
  | Const of const
  | Num of int
  | Var of Ident.t
  | Fun of Ident.t * t
  | App of t * t
  | Ifz of t * t * t
  | Pair of t * t
  | Fst of t
  | Snd of t

let mkTerm d = { desc = d; loc = None }

let is_value (term: t) : bool =
  match term.desc with
  | Const _ -> false
  | Num _ -> true
  | Var _ -> true
  | Fun (_,_) -> false
  | App (_,_) -> false
  | Ifz (_,_,_) -> false
  | Pair _ -> false
  | Fst _ -> false
  | Snd _ -> false

let rec free_vars (term: t) : Ident.t list =
  let abs x l = List.filter l ~f:(fun z -> not (z = x)) in
  match term.desc with
  | Const _ 
  | Num _ -> []
  | Var x -> [x]
  | Fun (x, t) -> abs x (free_vars t)
  | App (s, t) -> (free_vars s) @ (free_vars t)
  | Ifz (s, t1, t2) -> free_vars s @ free_vars t1 @ free_vars t2
  | Pair (s, t) -> (free_vars s) @ (free_vars t)
  | Fst s -> (free_vars s)
  | Snd s -> (free_vars s)

let rec all_vars (term: t) : Ident.t list =
  match term.desc with
  | Const _ 
  | Num _ -> []
  | Var x -> [x]
  | Fun (x, t) -> x :: (all_vars t)
  | App (s, t) -> (all_vars s) @ (all_vars t)
  | Ifz (s, t1, t2) -> all_vars s @ all_vars t1 @ all_vars t2
  | Pair (s, t) -> (all_vars s) @ (all_vars t)
  | Fst s -> (all_vars s)
  | Snd s -> (all_vars s)

let rename_vars (f: Ident.t -> Ident.t) (term: t) : t =
  let rec rn term =
    match term.desc with
    | Const _ 
    | Num _ -> term
    | Var x -> { term with desc = (Var(f x)) }
    | Fun (x, t) -> { term with desc = (Fun(f x, rn t)) }
    | App (s, t) -> { term with desc = (App(rn s, rn t)) }
    | Ifz (s, t1, t2) -> { term with desc = (Ifz(rn s, rn t1, rn t2)) }
    | Pair (s, t) -> { term with desc = (Pair(rn s, rn t)) }
    | Fst t -> { term with desc = (Fst(rn t)) }
    | Snd t -> { term with desc = (Fst(rn t)) }
  in rn term

let variant = rename_vars Ident.variant

(* Substitues [s] for [x].
   Returns [None] if [t] does not contain [x].
   If [head] is true then only the head occurrence is subtituted.
*)
let substitute ?head:(head=false) (s: t) (x: Ident.t) (t: t) : t option =
  (* Below sigma is always a permutation that maps bound
   * variables of t to suitably fresh variables. *)
  let fvs = free_vars s in
  let apply sigma y =
    List.Assoc.find sigma y
    |> Option.value ~default:y in
  let substituted = ref false in
  let rec sub sigma term =
    match term.desc with
    | Var(y) ->
      (* substitute only once if head *)
      if x = y && ((not head) || (not !substituted)) then
        (substituted := true; s)
      else
        { term with desc = Var(apply sigma y) }
    | Const _ 
    | Num _ -> term
    | Fun (x, t) -> 
      let (x', t') = abs sigma (x, t) in
      { term with desc = Fun(x', t') }
    | App (s, t) -> 
      { term with desc = App(sub sigma s, sub sigma t) }
    | Ifz (s, t1, t2) -> 
      { term with desc = Ifz(sub sigma s, sub sigma t1, sub sigma t2) }
    | Pair (s, t) -> 
      { term with desc = Pair(sub sigma s, sub sigma t) }
    | Fst t -> 
      { term with desc = Fst(sub sigma t) }
    | Snd t -> 
      { term with desc = Snd(sub sigma t) }
  and abs sigma (y, u) =
    match abs_list sigma ([y], u) with
    | [y'], u -> y', u
    | _ -> assert false
  and abs_list sigma (l, t1) =
    if List.mem l x then (l, t1)
    else if List.for_all l ~f:(fun y -> not (List.mem fvs y)) then
      (* no capture *)
      (l, sub sigma t1)
    else
      (* avoid capture *)
      let l' = List.map ~f:Ident.variant l in
      (l', sub ((List.zip_exn l l') @ sigma) t1)
  in
  let result = sub [] t in
  if (!substituted) then Some result else None

let head_subst (s: t) (x: Ident.t) (t: t) : t option =
  substitute ~head:true s x t

let subst (s: t) (x: Ident.t) (t: t) : t =
  match substitute ~head:false s x t with
  | None -> t
  | Some t' -> t'

let freshen_type_vars t = t
