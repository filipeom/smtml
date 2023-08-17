module BV = Value.BV
module FP = Value.FP

type sz =
  | S32
  | S64

type _ ty =
  | IntTy : int ty
  | RealTy : float ty
  | BoolTy : bool ty
  | StrTy : string ty
  | BvTy : sz -> BV.t ty
  | FpTy : sz -> FP.t ty

type iunop =
  | Neg
  | Not

type funop =
  | Neg
  | Abs
  | Sqrt
  | Nearest
  | Is_nan
  | Ceil
  | Floor

type bunop = Not

type sunop =
  | Len
  | Trim

type _ unop =
  | Int : iunop -> int unop
  | Flt : funop -> float unop
  | Bool : bunop -> bool unop
  | Str : sunop -> string unop
  | Bv : (iunop, iunop) BV.op -> BV.t unop
  | Fp : (funop, funop) FP.op -> FP.t unop

type ibinop =
  | Add
  | Sub
  | Mul
  | Div
  | DivU
  | Rem
  | RemU
  | Shl
  | Shr
  | ShrU
  | And
  | Or
  | Xor
  | Rotl
  | Rotr
  | ExtendS
  | ExtendU

type fbinop =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Min
  | Max

type bbinop =
  | And
  | Or
  | Xor

type sbinop =
  | Nth
  | Concat

type _ binop =
  | Int : ibinop -> int binop
  | Flt : fbinop -> float binop
  | Bool : bbinop -> bool binop
  | Str : sbinop -> string binop
  | Bv : (ibinop, ibinop) BV.op -> BV.t binop
  | Fp : (fbinop, fbinop) FP.op -> FP.t binop

type irelop =
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | LtU
  | LeU
  | GtU
  | GeU

type frelop =
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge

type brelop =
  | Eq
  | Ne

type _ relop =
  | Int : irelop -> int relop
  | Flt : frelop -> float relop
  | Bool : brelop -> bool relop
  | Str : brelop -> string relop
  | Bv : (irelop, irelop) BV.op -> BV.t relop
  | Fp : (frelop, frelop) FP.op -> FP.t relop

type btriop = Ite
type striop = Sub_str

type _ triop =
  | Bool : btriop -> bool triop
  | Str : striop -> string triop

type (_, _) icvtop =
  | TruncSF32 : (FP.t, BV.t) icvtop
  | TruncUF32 : (FP.t, BV.t) icvtop
  | TruncSF64 : (FP.t, BV.t) icvtop
  | TruncUF64 : (FP.t, BV.t) icvtop
  | ReinterpretFloat : ('a, 'r) icvtop
  | WrapI64 : (BV.t, BV.t) icvtop
  | ExtendSI32 : (BV.t, BV.t) icvtop
  | ExtendUI32 : (BV.t, BV.t) icvtop
  | ToBool : (BV.t, bool) icvtop
  | OfBool : (bool, BV.t) icvtop
  | ToString : ('a, string) icvtop
  | OfString : (string, 'r) icvtop

type (_, _) fcvtop =
  | DemoteF64 : (FP.t, FP.t) fcvtop
  | ConvertSI32 : (BV.t, FP.t) fcvtop
  | ConvertUI32 : (BV.t, FP.t) fcvtop
  | ConvertSI64 : (BV.t, FP.t) fcvtop
  | ConvertUI64 : (BV.t, FP.t) fcvtop
  | ReinterpretInt : (BV.t, FP.t) fcvtop
  | PromoteF32 : (FP.t, FP.t) fcvtop
  | ToString : ('a, string) fcvtop
  | OfString : (string, 'r) fcvtop

type (_, _) cvtop =
  | Int : ('a, 'r) icvtop -> ('a, 'r) cvtop
  | Flt : ('a, 'r) fcvtop -> ('a, 'r) cvtop
  | Bv : (('a, 'r) icvtop, ('a, 'r) icvtop) BV.op -> ('a, 'r) cvtop
  | Fp : (('a, 'r) fcvtop, ('a, 'r) fcvtop) FP.op -> ('a, 'r) cvtop

let negate_irelop : irelop -> irelop = function
  | Eq -> Ne
  | Ne -> Eq
  | Lt -> Ge
  | Le -> Gt
  | Gt -> Le
  | Ge -> Lt
  | LtU -> GeU
  | LeU -> GtU
  | GtU -> LeU
  | GeU -> LtU

let negate_frelop : frelop -> frelop = function
  | Eq -> Ne
  | Ne -> Eq
  | Lt -> Ge
  | Le -> Gt
  | Gt -> Le
  | Ge -> Lt

let negate_brelop : brelop -> brelop = function
  | Eq -> Ne
  | Ne -> Eq

module Pp : sig
  val pp_ty : _ ty -> string
  val pp_unop : _ unop -> string
  val pp_binop : _ binop -> string
  val pp_relop : _ relop -> string
  val pp_triop : _ triop -> string
  val pp_cvtop : (_, _) cvtop -> string
end = struct
  let pp_sz = function S32 -> "32" | S64 -> "64"

  let pp_ty (type a) (ty : a ty) : string =
    match ty with
    | IntTy -> "int"
    | RealTy -> "real"
    | BoolTy -> "bool"
    | StrTy -> "str"
    | BvTy sz -> Format.sprintf "i%s" (pp_sz sz)
    | FpTy sz -> Format.sprintf "f%s" (pp_sz sz)

  let pp_iunop : iunop -> string = function Neg -> "neg" | Not -> "not"

  let pp_funop : funop -> string = function
    | Neg -> "neg"
    | Abs -> "abs"
    | Sqrt -> "sqrt"
    | Nearest -> "nearest"
    | Is_nan -> "is_nan"
    | Ceil -> "ceil"
    | Floor -> "floor"

  let pp_unop (type a) (op : a unop) : string =
    match op with
    | Int op -> Format.sprintf "int.%s" (pp_iunop op)
    | Flt op -> Format.sprintf "flt.%s" (pp_funop op)
    | Bool Not -> "int.not"
    | Str op ->
      let op' = match op with Len -> "len" | Trim -> "trim" in
      Format.sprintf "str.%s" op'
    | Bv op -> (
      match op with
      | S32 op -> Format.sprintf "i32.%s" (pp_iunop op)
      | S64 op -> Format.sprintf "i64.%s" (pp_iunop op) )
    | Fp op -> (
      match op with
      | S32 op -> Format.sprintf "f32.%s" (pp_funop op)
      | S64 op -> Format.sprintf "f64.%s" (pp_funop op) )

  let pp_binop _ = assert false
  let pp_relop _ = assert false
  let pp_triop _ = assert false
  let pp_cvtop _ = assert false
end
