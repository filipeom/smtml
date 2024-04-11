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

type t =
  | S :
      { ty : 'a Ty.t
      ; name : string
      }
      -> t

val ( @: ) : string -> 'a Ty.t -> t

val make : 'a Ty.t -> string -> t

val mk_symbol : 'a Ty.t -> string -> t [@@deprecated "Please use 'make' instead"]

val equal : t -> t -> Bool.t

val rename : t -> string -> t

val type_of : t -> 'a Ty.t

val to_string : t -> string

val compare : t -> t -> int

val pp : Format.formatter -> t -> unit
