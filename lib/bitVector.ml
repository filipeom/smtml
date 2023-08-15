open Expression
open Types

module I32 = struct
  type num = Num.t

  let const (i : int32) : num t = Val (Num (I32 i))
  let mk_not (e : num t) : num t = Unop (I32 Not, e)
  let mk_clz (e : num t) : num t = Unop (I32 Clz, e)
  let mk_add (e1 : num t) (e2 : num t) : num t = Binop (I32 Add, e1, e2)
  let mk_sub (e1 : num t) (e2 : num t) : num t = Binop (I32 Sub, e1, e2)
  let mk_mul (e1 : num t) (e2 : num t) : num t = Binop (I32 Mul, e1, e2)
  let mk_div_u (e1 : num t) (e2 : num t) : num t = Binop (I32 DivU, e1, e2)
  let mk_div_s (e1 : num t) (e2 : num t) : num t = Binop (I32 DivS, e1, e2)
  let mk_rem_u (e1 : num t) (e2 : num t) : num t = Binop (I32 RemU, e1, e2)
  let mk_rem_s (e1 : num t) (e2 : num t) : num t = Binop (I32 RemS, e1, e2)
  let mk_shl (e1 : num t) (e2 : num t) : num t = Binop (I32 Shl, e1, e2)
  let mk_shr_u (e1 : num t) (e2 : num t) : num t = Binop (I32 ShrU, e1, e2)
  let mk_shr_s (e1 : num t) (e2 : num t) : num t = Binop (I32 ShrS, e1, e2)
  let mk_rotl (e1 : num t) (e2 : num t) : num t = Binop (I32 Rotl, e1, e2)
  let mk_rotr (e1 : num t) (e2 : num t) : num t = Binop (I32 Rotr, e1, e2)
  let mk_and (e1 : num t) (e2 : num t) : num t = Binop (I32 And, e1, e2)
  let mk_or (e1 : num t) (e2 : num t) : num t = Binop (I32 Or, e1, e2)
  let mk_xor (e1 : num t) (e2 : num t) : num t = Binop (I32 Xor, e1, e2)
  let mk_eq (e1 : num t) (e2 : num t) : bool t = Relop (I32 Eq, e1, e2)
  let mk_ne (e1 : num t) (e2 : num t) : bool t = Relop (I32 Ne, e1, e2)
  let mk_lt_u (e1 : num t) (e2 : num t) : bool t = Relop (I32 LtU, e1, e2)
  let mk_lt_s (e1 : num t) (e2 : num t) : bool t = Relop (I32 LtS, e1, e2)
  let mk_le_u (e1 : num t) (e2 : num t) : bool t = Relop (I32 LeU, e1, e2)
  let mk_le_s (e1 : num t) (e2 : num t) : bool t = Relop (I32 LeS, e1, e2)
  let mk_gt_u (e1 : num t) (e2 : num t) : bool t = Relop (I32 GtU, e1, e2)
  let mk_gt_s (e1 : num t) (e2 : num t) : bool t = Relop (I32 GtS, e1, e2)
  let mk_ge_u (e1 : num t) (e2 : num t) : bool t = Relop (I32 GeU, e1, e2)
  let mk_ge_s (e1 : num t) (e2 : num t) : bool t = Relop (I32 GeS, e1, e2)
end

module I64 = struct
  type num = Num.t

  let const (i : int64) : num t = Val (Num (I64 i))
  let mk_not (e : num t) : num t = Unop (I64 Not, e)
  let mk_clz (e : num t) : num t = Unop (I64 Clz, e)
  let mk_add (e1 : num t) (e2 : num t) : num t = Binop (I64 Add, e1, e2)
  let mk_sub (e1 : num t) (e2 : num t) : num t = Binop (I64 Sub, e1, e2)
  let mk_mul (e1 : num t) (e2 : num t) : num t = Binop (I64 Mul, e1, e2)
  let mk_div_u (e1 : num t) (e2 : num t) : num t = Binop (I64 DivU, e1, e2)
  let mk_div_s (e1 : num t) (e2 : num t) : num t = Binop (I64 DivS, e1, e2)
  let mk_rem_u (e1 : num t) (e2 : num t) : num t = Binop (I64 RemU, e1, e2)
  let mk_rem_s (e1 : num t) (e2 : num t) : num t = Binop (I64 RemS, e1, e2)
  let mk_shl (e1 : num t) (e2 : num t) : num t = Binop (I64 Shl, e1, e2)
  let mk_shr_u (e1 : num t) (e2 : num t) : num t = Binop (I64 ShrU, e1, e2)
  let mk_shr_s (e1 : num t) (e2 : num t) : num t = Binop (I64 ShrS, e1, e2)
  let mk_rotl (e1 : num t) (e2 : num t) : num t = Binop (I64 Rotl, e1, e2)
  let mk_rotr (e1 : num t) (e2 : num t) : num t = Binop (I64 Rotr, e1, e2)
  let mk_and (e1 : num t) (e2 : num t) : num t = Binop (I64 And, e1, e2)
  let mk_or (e1 : num t) (e2 : num t) : num t = Binop (I64 Or, e1, e2)
  let mk_xor (e1 : num t) (e2 : num t) : num t = Binop (I64 Xor, e1, e2)
  let mk_eq (e1 : num t) (e2 : num t) : bool t = Relop (I64 Eq, e1, e2)
  let mk_ne (e1 : num t) (e2 : num t) : bool t = Relop (I64 Ne, e1, e2)
  let mk_lt_u (e1 : num t) (e2 : num t) : bool t = Relop (I64 LtU, e1, e2)
  let mk_lt_s (e1 : num t) (e2 : num t) : bool t = Relop (I64 LtS, e1, e2)
  let mk_le_u (e1 : num t) (e2 : num t) : bool t = Relop (I64 LeU, e1, e2)
  let mk_le_s (e1 : num t) (e2 : num t) : bool t = Relop (I64 LeS, e1, e2)
  let mk_gt_u (e1 : num t) (e2 : num t) : bool t = Relop (I64 GtU, e1, e2)
  let mk_gt_s (e1 : num t) (e2 : num t) : bool t = Relop (I64 GtS, e1, e2)
  let mk_ge_u (e1 : num t) (e2 : num t) : bool t = Relop (I64 GeU, e1, e2)
  let mk_ge_s (e1 : num t) (e2 : num t) : bool t = Relop (I64 GeS, e1, e2)
end