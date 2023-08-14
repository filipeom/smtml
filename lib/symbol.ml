open Core

type 'a t = S : 'a Type.ty * string -> 'a t
type symbol = Sym : 'a t -> symbol

let mk_symbol_int (name : string) : int t = S (Type.IntTy, name)
let mk_symbol_real (name : string) : float t = S (Type.RealTy, name)
let mk_symbol_bool (name : string) : bool t = S (Type.BoolTy, name)
let mk_symbol_str (name : string) : string t = S (Type.StrTy, name)
let mk_symbol_num (name : string) : Num.t t = S (Type.NumTy, name)

let equal (type a) (S (_, x1) : a t) (S (_, x2) : a t) : bool =
  String.equal x1 x2

let rename (S (ty, _) : _ t) (x : string) = S (ty, x)
let type_of (type a) (S (ty, _) : a t) : a Type.ty = ty

module Pp = struct
  let pp (S (_, name) : _ t) : string = name
  let pp_symbol (Sym s) : string = pp s
end
