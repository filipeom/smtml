open Core
open Expression
open Types

module F32 = struct
  type num = Num.t

  let const (f : float) : num t = Val (Num (F32 (Int32.bits_of_float f)))
  let mk_neg (e : num t) : num t = Unop (F32 Neg, e)
  let mk_abs (e : num t) : num t = Unop (F32 Abs, e)
  let mk_sqrt (e : num t) : num t = Unop (F32 Sqrt, e)
  let mk_nearest (e : num t) : num t = Unop (F32 Nearest, e)
  let mk_is_nan (e : num t) : bool t = Unop (F32 IsNan, e)
  let mk_add (e1 : num t) (e2 : num t) : num t = Binop (F32 Add, e1, e2)
  let mk_sub (e1 : num t) (e2 : num t) : num t = Binop (F32 Sub, e1, e2)
  let mk_mul (e1 : num t) (e2 : num t) : num t = Binop (F32 Mul, e1, e2)
  let mk_div (e1 : num t) (e2 : num t) : num t = Binop (F32 Div, e1, e2)
  let mk_min (e1 : num t) (e2 : num t) : num t = Binop (F32 Min, e1, e2)
  let mk_max (e1 : num t) (e2 : num t) : num t = Binop (F32 Max, e1, e2)
  let mk_rem (e1 : num t) (e2 : num t) : num t = Binop (F32 Rem, e1, e2)
  let mk_eq (e1 : num t) (e2 : num t) : bool t = Relop (F32 Eq, e1, e2)
  let mk_ne (e1 : num t) (e2 : num t) : bool t = Relop (F32 Ne, e1, e2)
  let mk_lt (e1 : num t) (e2 : num t) : bool t = Relop (F32 Lt, e1, e2)
  let mk_le (e1 : num t) (e2 : num t) : bool t = Relop (F32 Le, e1, e2)
  let mk_gt (e1 : num t) (e2 : num t) : bool t = Relop (F32 Gt, e1, e2)
  let mk_ge (e1 : num t) (e2 : num t) : bool t = Relop (F32 Ge, e1, e2)
end

module F64 = struct
  type num = Num.t

  let const (f : float) : num t = Val (Num (F64 (Int64.bits_of_float f)))
  let mk_neg (e : num t) : num t = Unop (F64 Neg, e)
  let mk_abs (e : num t) : num t = Unop (F64 Abs, e)
  let mk_sqrt (e : num t) : num t = Unop (F64 Sqrt, e)
  let mk_nearest (e : num t) : num t = Unop (F64 Nearest, e)
  let mk_is_nan (e : num t) : bool t = Unop (F64 IsNan, e)
  let mk_add (e1 : num t) (e2 : num t) : num t = Binop (F64 Add, e1, e2)
  let mk_sub (e1 : num t) (e2 : num t) : num t = Binop (F64 Sub, e1, e2)
  let mk_mul (e1 : num t) (e2 : num t) : num t = Binop (F64 Mul, e1, e2)
  let mk_div (e1 : num t) (e2 : num t) : num t = Binop (F64 Div, e1, e2)
  let mk_min (e1 : num t) (e2 : num t) : num t = Binop (F64 Min, e1, e2)
  let mk_max (e1 : num t) (e2 : num t) : num t = Binop (F64 Max, e1, e2)
  let mk_rem (e1 : num t) (e2 : num t) : num t = Binop (F64 Rem, e1, e2)
  let mk_eq (e1 : num t) (e2 : num t) : bool t = Relop (F64 Eq, e1, e2)
  let mk_ne (e1 : num t) (e2 : num t) : bool t = Relop (F64 Ne, e1, e2)
  let mk_lt (e1 : num t) (e2 : num t) : bool t = Relop (F64 Lt, e1, e2)
  let mk_le (e1 : num t) (e2 : num t) : bool t = Relop (F64 Le, e1, e2)
  let mk_gt (e1 : num t) (e2 : num t) : bool t = Relop (F64 Gt, e1, e2)
  let mk_ge (e1 : num t) (e2 : num t) : bool t = Relop (F64 Ge, e1, e2)
end
