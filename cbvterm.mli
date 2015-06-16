(** Typed representation of intput terms *)

module Ident = Intlib.Ident
module Basetype = Intlib.Basetype

type const = Ast.const
               
type t = {
  t_desc: t_desc;
  t_type: Cbvtype.t;
  t_context: (Ident.t * Cbvtype.t) list;
  t_loc: Ast.Location.t
} and t_desc =
  | Var of Ident.t
  | Const of const
  | Num of int
  | Fun of (Ident.t list * Cbvtype.t) * t
  | App of t * t
  | Ifz of t * t * t
  | Pair of t * t
  | Fst of t
  | Snd of t
