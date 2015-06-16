open Intlib
(** Source terms *)
  
(** Location of term in the source file *)
module Location : sig
  type pos = { column: int; line: int}
  type loc = {start_pos: pos; end_pos: pos}
  type t = loc option
  val none: t
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

val mkTerm : t_desc -> t

(** Check if a term conforms to the grammar of value terms. *)
val is_value: t -> bool

(** Free variables *)
val free_vars : t -> Ident.t list

(** All variables, free and bound *)
val all_vars : t -> Ident.t list

(** Rename variables using given function. *)
val rename_vars : (Ident.t -> Ident.t) -> t -> t

(** Compute variant of the term. *)
val variant : t -> t

(** Head substitution.
    [head_subst s x t] substitues [s] for the head occurrence of [x],
    if one exists. It returns [None] if [t] does not contain [x]. *)
val head_subst: t -> Ident.t -> t -> t option

(** Capture avoiding substitution.
    [subst s x t] substitues [s] for [x] in [t]. *)
val subst: t -> Ident.t -> t -> t

(** Rename type variables in type annotations with fresh type variables. *)
val freshen_type_vars : t -> t
