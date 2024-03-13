type _ cast =
  | C8 : int cast
  | C32 : int32 cast
  | C64 : int64 cast

type _ t' =
  | Ty_int : [ `Ty_int ] t'
  | Ty_real : [ `Ty_real ] t'
  | Ty_bool : [ `Ty_bool ] t'
  | Ty_str : [ `Ty_str ] t'
  | Ty_bitv : int -> [ `Ty_bitv ] t'
  | Ty_fp : int -> [ `Ty_fp ] t'

type t = T : 'a t' -> t

type _ unop =
  | Abs : [< `Ty_real | `Ty_fp ] unop
  | Ceil : [< `Ty_real | `Ty_fp ] unop
  | Clz : [ `Ty_bitv ] unop
  | Ctz : [ `Ty_bitv ] unop
  | Floor : [< `Ty_real | `Ty_fp ] unop
  | Is_nan : [ `Ty_fp ] unop
  | Len : [ `Ty_str ] unop
  | Neg : [< `Ty_int | `Ty_real | `Ty_bitv | `Ty_fp ] unop
  | Nearest : [< `Ty_real | `Ty_fp ] unop
  | Not : [< `Ty_bitv | `Ty_bool ] unop
  | Sqrt : [< `Ty_real | `Ty_fp ] unop
  | Trim : [ `Ty_str ] unop
  | Trunc : [< `Ty_real | `Ty_fp ] unop

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
  (* To remove *)
  | Nth
  | Concat

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

type _ triop =
  | Ite : [ `Ty_bool ] triop
  | Substr : [ `Ty_str ] triop

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
  | ExtS of int
  | ExtU of int
  | String_to_code
  | String_from_code

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

val pp_unop : Format.formatter -> _ unop -> unit

val pp_binop : Format.formatter -> binop -> unit

val pp_triop : Format.formatter -> _ triop -> unit

val pp_relop : Format.formatter -> relop -> unit

val pp_cvtop : Format.formatter -> cvtop -> unit

val pp : Format.formatter -> t -> unit

val pp_logic : Format.formatter -> logic -> unit

val equal : t -> t -> bool

val string_of_type : t -> string

val size : t -> int
