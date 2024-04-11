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

let ( @: ) (name : string) (ty : 'a Ty.t) : t = S { name; ty }

let make (ty : 'a Ty.t) (name : string) : t = name @: ty

let mk_symbol (ty : 'a Ty.t) (name : string) : t = name @: ty

let equal (S s1 : t) (S s2 : t) : bool =
  match (s1.ty, s2.ty) with
  | Ty.Ty_int, Ty.Ty_int
  | Ty.Ty_real, Ty.Ty_real
  | Ty.Ty_bool, Ty.Ty_bool
  | Ty.Ty_str, Ty.Ty_str
  | Ty.Ty_list, Ty.Ty_list ->
    String.equal s1.name s2.name
  | Ty.Ty_bitv n1, Ty.Ty_bitv n2 | Ty.Ty_fp n1, Ty.Ty_fp n2 ->
    n1 = n2 && String.equal s1.name s2.name
  | _ -> false

let compare (S t1 : t) (S t2 : t) : int =
  let compare_name = compare t1.name t2.name in
  compare_name
(* if compare_name = 0 then compare t1.ty t2.ty else compare_name *)

let rename (S symbol : t) (name : string) : t = S { symbol with name }

let type_of (S { ty = _; _ } : t) : 'a Ty.t = assert false

let pp (fmt : Format.formatter) (S { name; _ } : t) : unit =
  Format.pp_print_string fmt name

let to_string (S { name; _ } : t) : string = name
