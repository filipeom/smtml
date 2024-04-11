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

type _ cast =
  | C8 : int cast
  | C32 : int32 cast
  | C64 : int64 cast

type 'a t =
  | Ty_int : [ `Ty_int ] t
  | Ty_real : [ `Ty_real ] t
  | Ty_bool : [ `Ty_bool ] t
  | Ty_str : [ `Ty_str ] t
  | Ty_bitv : int -> [ `Ty_bitv ] t
  | Ty_fp : int -> [ `Ty_fp ] t
  | Ty_list : [ `Ty_list ] t
  | Ty_tuple : [ `Ty_tuple ] t
  | Ty_array : [ `Ty_array ] t

type 'a unop =
  | Neg : [< `Ty_int | `Ty_real | `Ty_bitv | `Ty_fp ] unop
  | Not : [< `Ty_bitv | `Ty_bool ] unop
  | Clz : [> `Ty_bitv ] unop
  | Ctz : [> `Ty_bitv ] unop
  (* Float *)
  | Abs : [< `Ty_real | `Ty_fp ] unop
  | Sqrt : [< `Ty_real | `Ty_fp ] unop
  | Is_nan : [< `Ty_real | `Ty_fp ] unop
  | Ceil : [< `Ty_real | `Ty_fp ] unop
  | Floor : [< `Ty_real | `Ty_fp ] unop
  | Trunc : [< `Ty_real | `Ty_fp ] unop
  | Nearest : [< `Ty_real | `Ty_fp ] unop
  (* String *)
  | Seq_length : [> `Ty_str ] unop (* (str.len String Int) *)
  | Trim : [> `Ty_str ] unop (* uninterpreted *)

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | DivU
  | Rem
  | RemU
  | Shl
  | ShrA
  | ShrL
  | And
  | Or
  | Xor
  | Pow
  | Min
  | Max
  | Rotl
  | Rotr
  (* String *)
  | Seq_at (* (str.at String Int String) *)
  | Seq_concat (* (str.substr String Int Int String) *)
  | Seq_prefix (* (str.prefixof String String Bool) *)
  | Seq_suffix (* (str.suffixof String String Bool) *)
  | Seq_contains (* (str.contains String String Bool) *)
  | Seq_last_index

type relop =
  | Eq
  | Ne
  | Lt
  | LtU
  | Gt
  | GtU
  | Le
  | LeU
  | Ge
  | GeU

type triop =
  | Ite
  (* String *)
  | Seq_extract (* (str.substr String Int Int String) *)
  | Seq_replace (* (str.replace String String String String) *)
  | Seq_index (* (str.indexof String String Int Int) *)

type cvtop =
  | ToString
  | OfString
  | ToBool
  | OfBool
  | Reinterpret_int
  | Reinterpret_float
  | DemoteF64
  | PromoteF32
  | ConvertSI32
  | ConvertUI32
  | ConvertSI64
  | ConvertUI64
  | TruncSF32
  | TruncUF32
  | TruncSF64
  | TruncUF64
  | WrapI64
  | Sign_extend of int
  | Zero_extend of int
  (* String *)
  | String_to_code (* (str.to_code String Int) *)
  | String_from_code (* (str.from_code Int String) *)
  | String_to_int (* (str.to_int String Int) *)
  | String_from_int (* (str.from_int Int String) *)

type logic =
  | AUFLIA
  | AUFLIRA
  | AUFNIRA
  | LIA
  | LRA
  | QF_ABV
  | QF_AUFBV
  | QF_AUFLIA
  | QF_AX
  | QF_BV
  | QF_BVFP
  | QF_IDL
  | QF_LIA
  | QF_LRA
  | QF_NIA
  | QF_NRA
  | QF_RDL
  | QF_UF
  | QF_UFBV
  | QF_UFIDL
  | QF_UFLIA
  | QF_UFLRA
  | QF_UFNRA
  | UFLRA
  | UFNIA

val pp_unop : Format.formatter -> 'a unop -> unit

val pp_binop : Format.formatter -> binop -> unit

val pp_triop : Format.formatter -> triop -> unit

val pp_relop : Format.formatter -> relop -> unit

val pp_cvtop : Format.formatter -> cvtop -> unit

val pp : Format.formatter -> 'a t -> unit

val pp_logic : Format.formatter -> logic -> unit

val equal : 'a t -> 'a t -> bool

val string_of_type : 'a t -> string

val size : 'a t -> int
