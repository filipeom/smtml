(***************************************************************************)
(* This file is part of the third-party OCaml library `smtml`.             *)
(* Copyright (C) 2023-2024 formalsec                                       *)
(*                                                                         *)
(* This program is free software: you can redistribute it and/or modify    *)
(* it under the terms of the GNU General Public License as published by    *)
(* the Free Software Foundation, either version 3 of the License, or       *)
(* (at your option) any later version.                                     *)
(*                                                                         *)
(* This program is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(* GNU General Public License for more details.                            *)
(*                                                                         *)
(* You should have received a copy of the GNU General Public License       *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(***************************************************************************)

(** Term definitions of the abstract syntax *)
type t = E : 'a expr Hc.hash_consed -> t

and 'a expr =
  | Val of Value.t
  | Ptr of int32 * t
  | Symbol of Symbol.t
  | List of t list
  | Array of t array
  | Tuple of t list
  | App : [> `Op of string ] * t list -> 'a expr
  | Unop : 'a Ty.t * 'a Ty.unop * t -> 'a expr
  | Binop : 'a Ty.t * Ty.binop * t * t -> 'a expr
  | Triop : 'a Ty.t * Ty.triop * t * t * t -> 'a expr
  | Relop : 'a Ty.t * Ty.relop * t * t -> 'a expr
  | Cvtop : 'a Ty.t * Ty.cvtop * t -> 'a expr
  | Extract of t * int * int
  | Concat of t * t

val equal : t -> t -> bool

val hash : t -> int

val make : 'a expr -> t

val ( @: ) : 'a expr -> 'a Ty.t -> t [@@deprecated "Please use 'make' instead"]

val view : t -> 'a expr

val ty : t -> 'a Ty.t

val mk_symbol : Symbol.t -> t

val is_symbolic : t -> bool

val get_symbols : t list -> Symbol.t list

val negate_relop : t -> (t, string) Result.t

val pp : Format.formatter -> t -> unit

val pp_smt : Format.formatter -> t list -> unit

val pp_list : Format.formatter -> t list -> unit

val to_string : t -> string

val value : Value.t -> t

(** Smart unop constructor, applies simplifications at constructor level *)
val unop : 'a Ty.t -> 'a Ty.unop -> t -> t

(** Dumb unop constructor, no simplifications *)
val unop' : 'a Ty.t -> 'a Ty.unop -> t -> t

(** Smart binop constructor, applies simplifications at constructor level *)
val binop : 'a Ty.t -> Ty.binop -> t -> t -> t

(** Dumb binop constructor, no simplifications *)
val binop' : 'a Ty.t -> Ty.binop -> t -> t -> t

(** Smart triop constructor, applies simplifications at constructor level *)
val triop : 'a Ty.t -> Ty.triop -> t -> t -> t -> t

(** Dumb triop constructor, no simplifications *)
val triop' : 'a Ty.t -> Ty.triop -> t -> t -> t -> t

(** Smart relop constructor, applies simplifications at constructor level *)
val relop : 'a Ty.t -> Ty.relop -> t -> t -> t

(** Dumb relop constructor, no simplifications *)
val relop' : 'a Ty.t -> Ty.relop -> t -> t -> t

(** Smart relop constructor, applies simplifications at constructor level *)
val cvtop : 'a Ty.t -> Ty.cvtop -> t -> t

(** Dumb cvtop constructor, no simplifications *)
val cvtop' : 'a Ty.t -> Ty.cvtop -> t -> t

(** Smart extract constructor, applies simplifications at constructor level *)
val extract : t -> high:int -> low:int -> t

(** Dumb extract constructor, no simplifications *)
val extract' : t -> high:int -> low:int -> t

(** Smart concat constructor, applies simplifications at constructor level *)
val concat : t -> t -> t

(** Dumb concat constructor, no simplifications *)
val concat' : t -> t -> t

(** Applies expression simplifications until a fixpoint *)
val simplify : t -> t

module Hc : sig
  val clear : unit -> unit

  val stats : unit -> Hashtbl.statistics

  val length : unit -> int
end

module Bool : sig
  val v : bool -> t

  val not : t -> t

  val ( = ) : t -> t -> t

  val distinct : t -> t -> t

  val and_ : t -> t -> t

  val or_ : t -> t -> t

  val ite : t -> t -> t -> t
end

module Bitv : sig
  module I8 : Constructors_intf.Infix with type elt := int and type t := t

  module I32 : Constructors_intf.Infix with type elt := int32 and type t := t

  module I64 : Constructors_intf.Infix with type elt := int64 and type t := t
end

module Fpa : sig
  module F32 : Constructors_intf.Infix with type elt := float and type t := t

  module F64 : Constructors_intf.Infix with type elt := float and type t := t
end
