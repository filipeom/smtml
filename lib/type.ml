module BV = Value.BV

type _ ty =
  | IntTy : int ty
  | RealTy : float ty
  | BoolTy : bool ty
  | NumTy : Num.t ty
  | StrTy : string ty

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
  | I32 : iunop -> BV.t unop
  | I64 : iunop -> BV.t unop
  | F32 : funop -> Num.t unop
  | F64 : funop -> Num.t unop

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
  | I32 : ibinop -> BV.t binop
  | I64 : ibinop -> BV.t binop
  | F32 : fbinop -> Num.t binop
  | F64 : fbinop -> Num.t binop

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
  | I32 : irelop -> BV.t relop
  | I64 : irelop -> BV.t relop
  | F32 : frelop -> Num.t relop
  | F64 : frelop -> Num.t relop

type btriop = Ite
type striop = Sub_str

type _ triop =
  | Bool : btriop -> bool triop
  | Str : striop -> string triop

type (_, _) icvtop =
  | TruncSF32 : (Num.t, BV.t) icvtop
  | TruncUF32 : (Num.t, BV.t) icvtop
  | TruncSF64 : (Num.t, BV.t) icvtop
  | TruncUF64 : (Num.t, BV.t) icvtop
  | ReinterpretFloat : ('a, 'r) icvtop
  | WrapI64 : (BV.t, BV.t) icvtop
  | ExtendSI32 : (BV.t, BV.t) icvtop
  | ExtendUI32 : (BV.t, BV.t) icvtop
  | ToBool : (BV.t, bool) icvtop
  | OfBool : (bool, BV.t) icvtop
  | ToString : ('a, string) icvtop
  | OfString : (string, 'r) icvtop

type (_, _) fcvtop =
  | DemoteF64 : (Num.t, Num.t) fcvtop
  | ConvertSI32 : (Num.t, Num.t) fcvtop
  | ConvertUI32 : (Num.t, Num.t) fcvtop
  | ConvertSI64 : (Num.t, Num.t) fcvtop
  | ConvertUI64 : (Num.t, Num.t) fcvtop
  | ReinterpretInt : (Num.t, Num.t) fcvtop
  | PromoteF32 : (Num.t, Num.t) fcvtop
  | ToString : ('a, string) fcvtop
  | OfString : (string, 'r) fcvtop

type (_, _) cvtop =
  | Int : ('a, 'r) icvtop -> ('a, 'r) cvtop
  | Flt : ('a, 'r) fcvtop -> ('a, 'r) cvtop
  | I32 : ('a, 'r) icvtop -> ('a, 'r) cvtop
  | I64 : ('a, 'r) icvtop -> ('a, 'r) cvtop
  | F32 : ('a, 'r) fcvtop -> ('a, 'r) cvtop
  | F64 : ('a, 'r) fcvtop -> ('a, 'r) cvtop

module Pp : sig
  val pp_ty : _ ty -> string
  val pp_unop : _ unop -> string
  val pp_binop : _ binop -> string
  val pp_relop : _ relop -> string
  val pp_triop : _ triop -> string
  val pp_cvtop : (_, _) cvtop -> string
end = struct
  let pp_ty (type a) (ty : a ty) : string =
    match ty with
    | IntTy -> "int"
    | RealTy -> "real"
    | BoolTy -> "bool"
    | StrTy -> "str"
    | NumTy -> "num"

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
    | I32 op -> Format.sprintf "i32.%s" (pp_iunop op)
    | I64 op -> Format.sprintf "i64.%s" (pp_iunop op)
    | F32 op -> Format.sprintf "f32.%s" (pp_funop op)
    | F64 op -> Format.sprintf "f64.%s" (pp_funop op)

  let pp_binop _ = assert false
  let pp_relop _ = assert false
  let pp_triop _ = assert false
  let pp_cvtop _ = assert false
end
