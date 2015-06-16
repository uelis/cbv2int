(** Representation of interactive types *)

open Core.Std
module Basetype = Intlib.Basetype
module Uftype = Intlib.Uftype                    

type 'a sgn =
  | Nat
  | Pair of 'a * 'a
  | Fun of 'a * (Basetype.t * Basetype.t * 'a)
with sexp

include Uftype.S with type 'a Sgn.t = 'a sgn

val to_string: ?concise:bool -> t -> string
