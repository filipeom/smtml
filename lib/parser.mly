%{
open Core
open Expr

(* let varmap = Hashtbl.create (module String) *)

(* let add_bind x t = Hashtbl.set varmap ~key:x ~data:t *)
(* let get_bind x = Hashtbl.find varmap x *)

let mk_symbol (type a) (ty : a Type.ty) x : a Symbol.t =
  match ty with
  | Type.IntTy -> Symbol.mk_symbol_int x
  | Type.RealTy -> Symbol.mk_symbol_real x
  | Type.BoolTy -> Symbol.mk_symbol_bool x
  | Type.StrTy -> Symbol.mk_symbol_str x
  | Type.BvTy S32 -> Symbol.mk_symbol_i32 x
  | Type.BvTy S64 -> Symbol.mk_symbol_i64 x
  | Type.FpTy S32 -> Symbol.mk_symbol_f32 x
  | Type.FpTy S64 -> Symbol.mk_symbol_f64 x
%}
%token LPAREN
%token RPAREN
%token INT_NEG INT_ADD INT_SUB INT_DIV INT_MUL INT_REM
%token INT_EQ INT_NE INT_LT INT_LE INT_GT INT_GE
%token REAL_NEG REAL_ABS REAL_SQRT REAL_CEIL REAL_FLOOR
%token REAL_ADD REAL_SUB REAL_DIV REAL_MUL REAL_MIN REAL_MAX
%token REAL_EQ REAL_NE REAL_LT REAL_LE REAL_GT REAL_GE
%token BOOL_NOT BOOL_AND BOOL_OR BOOL_XOR BOOL_EQ BOOL_NE BOOL_ITE
%token STR_LEN STR_TRIM STR_CONCAT STR_SUBSTR STR_EQ STR_NE
%token I32_NOT I32_NEG I32_ADD I32_SUB I32_DIV I32_DIVU I32_AND I32_ROTR
%token I32_OR I32_XOR I32_MUL I32_SHL I32_SHR I32_SHRU I32_REM I32_REMU I32_ROTL
%token I32_EQ I32_NE I32_LT I32_LTU I32_LE I32_LEU I32_GT I32_GTU I32_GE I32_GEU
%token I64_NOT I64_NEG I64_ADD I64_SUB I64_DIV I64_DIVU I64_AND I64_ROTR
%token I64_OR I64_XOR I64_MUL I64_SHL I64_SHR I64_SHRU I64_REM I64_REMU I64_ROTL
%token I64_EQ I64_NE I64_LT I64_LTU I64_LE I64_LEU I64_GT I64_GTU I64_GE I64_GEU
%token F32_NEG F32_ABS F32_SQRT F32_NEAREST F32_IS_NAN F32_ADD F32_SUB F32_MUL
%token F32_DIV F32_MIN F32_MAX F32_REM F32_EQ F32_NE F32_LT F32_LE F32_GT F32_GE
%token F64_NEG F64_ABS F64_SQRT F64_NEAREST F64_IS_NAN F64_ADD F64_SUB F64_MUL
%token F64_DIV F64_MIN F64_MAX F64_REM F64_EQ F64_NE F64_LT F64_LE F64_GT F64_GE
%token INT_TYPE REAL_TYPE BOOL_TYPE STR_TYPE
%token BV32_TYPE BV64_TYPE FP32_TYPE FP64_TYPE
%token DECLARE_FUN ASSERT CHECK_SAT GET_MODEL
(* %token HOLE *)
%token EOF

%token <int> NUM
%token <float> DEC
%token <bool> BOOL
%token <string> STR
%token <string> SYMBOL

%start <Ast.t list> script
%%

let script := stmts = list(stmt); EOF; { stmts }

let stmt :=
  | LPAREN; DECLARE_FUN; x = SYMBOL; INT_TYPE; RPAREN; {
      Ast.Declare (Symbol.Sym (mk_symbol Type.IntTy x))
    }
  | LPAREN; DECLARE_FUN; x = SYMBOL; REAL_TYPE; RPAREN; {
      Ast.Declare (Symbol.Sym (mk_symbol Type.RealTy x))
    }
  | LPAREN; DECLARE_FUN; x = SYMBOL; BOOL_TYPE; RPAREN; {
      Ast.Declare (Symbol.Sym (mk_symbol Type.BoolTy x))
    }
  | LPAREN; DECLARE_FUN; x = SYMBOL; STR_TYPE; RPAREN; {
      Ast.Declare (Symbol.Sym (mk_symbol Type.StrTy x))
    }
  | LPAREN; DECLARE_FUN; x = SYMBOL; BV32_TYPE; RPAREN; {
      Ast.Declare (Symbol.Sym (mk_symbol (Type.BvTy S32) x))
    }
  | LPAREN; DECLARE_FUN; x = SYMBOL; BV64_TYPE; RPAREN; {
      Ast.Declare (Symbol.Sym (mk_symbol (Type.BvTy S64) x))
    }
  | LPAREN; DECLARE_FUN; x = SYMBOL; FP32_TYPE; RPAREN; {
      Ast.Declare (Symbol.Sym (mk_symbol (Type.FpTy S32) x))
    }
  | LPAREN; DECLARE_FUN; x = SYMBOL; FP64_TYPE; RPAREN; {
      Ast.Declare (Symbol.Sym (mk_symbol (Type.FpTy S64) x))
    }
  | LPAREN; ASSERT; ~ = bexpr; RPAREN; { Ast.Assert bexpr }
  | LPAREN; CHECK_SAT; RPAREN; { Ast.CheckSat }
  | LPAREN; GET_MODEL; RPAREN; { Ast.GetModel }

let iexpr :=
  | n = NUM; { Integer.const n }
  | x = SYMBOL; { Symbol (mk_symbol Type.IntTy x) }
  | LPAREN; INT_NEG; ~ = iexpr; RPAREN; { Unop (Int Neg, iexpr) }
  | LPAREN; INT_ADD; e1 = iexpr; e2 = iexpr; RPAREN; { Binop (Int Add, e1, e2) }
  | LPAREN; INT_SUB; e1 = iexpr; e2 = iexpr; RPAREN; { Binop (Int Sub, e1, e2) }
  | LPAREN; INT_DIV; e1 = iexpr; e2 = iexpr; RPAREN; { Binop (Int Div, e1, e2) }
  | LPAREN; INT_MUL; e1 = iexpr; e2 = iexpr; RPAREN; { Binop (Int Mul, e1, e2) }
  | LPAREN; INT_REM; e1 = iexpr; e2 = iexpr; RPAREN; { Binop (Int Rem, e1, e2) }

let fexpr :=
  | n = DEC; { Val (Value.Real n) }
  | x = SYMBOL; { Symbol (mk_symbol Type.RealTy x) }
  | LPAREN; REAL_NEG; ~ = fexpr; RPAREN; { Unop (Flt Neg, fexpr) }
  | LPAREN; REAL_ABS; ~ = fexpr; RPAREN; { Unop (Flt Abs, fexpr) }
  | LPAREN; REAL_SQRT; ~ = fexpr; RPAREN; { Unop (Flt Sqrt, fexpr) }
  | LPAREN; REAL_CEIL; ~ = fexpr; RPAREN; { Unop (Flt Ceil, fexpr) }
  | LPAREN; REAL_FLOOR; ~ = fexpr; RPAREN; { Unop (Flt Floor, fexpr) }
  | LPAREN; REAL_ADD; e1 = fexpr; e2 = fexpr; RPAREN; { Binop (Flt Add, e1, e2) }
  | LPAREN; REAL_SUB; e1 = fexpr; e2 = fexpr; RPAREN; { Binop (Flt Sub, e1, e2) }
  | LPAREN; REAL_DIV; e1 = fexpr; e2 = fexpr; RPAREN; { Binop (Flt Div, e1, e2) }
  | LPAREN; REAL_MUL; e1 = fexpr; e2 = fexpr; RPAREN; { Binop (Flt Mul, e1, e2) }
  | LPAREN; REAL_MIN; e1 = fexpr; e2 = fexpr; RPAREN; { Binop (Flt Min, e1, e2) }
  | LPAREN; REAL_MAX; e1 = fexpr; e2 = fexpr; RPAREN; { Binop (Flt Max, e1, e2) }

let sexpr :=
  | v = STR; { Val (Value.Str v) }
  | x = SYMBOL; { Symbol (mk_symbol Type.StrTy x) }
  | LPAREN; STR_LEN; ~ = sexpr; RPAREN; { Unop (Str Len, sexpr) }
  | LPAREN; STR_TRIM; ~ = sexpr; RPAREN; { Unop (Str Trim, sexpr) }
  (* | LPAREN; STR_NTH; e1 = sexpr; e2 = iexpr; RPAREN; { Binop (Str Nth, e1, e2) } *)
  | LPAREN; STR_CONCAT; e1 = sexpr; e2 = sexpr; RPAREN;
    { Binop (Str Concat, e1, e2) }
  | LPAREN; STR_SUBSTR; e1 = sexpr; e2 = iexpr; e3 = iexpr; RPAREN;
    { Triop (Str Sub_str, e1, e2, e3) }

let bvexpr :=
  | LPAREN; BV32_TYPE; n = NUM; RPAREN; { Val (Bv (S32 (Int32.of_int_trunc n))) }
  | LPAREN; I32_NOT; ~ = bvexpr; RPAREN; { Unop (Bv (S32 Not), bvexpr) }
  | LPAREN; I32_NEG; ~ = bvexpr; RPAREN; { Unop (Bv (S32 Neg), bvexpr) }
  | LPAREN; I32_ADD; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S32 Add), e1, e2) }
  | LPAREN; I32_SUB; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S32 Sub), e1, e2) }
  | LPAREN; I32_DIV; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S32 Div), e1, e2) }
  | LPAREN; I32_DIVU; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S32 DivU), e1, e2) }
  | LPAREN; I32_AND; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S32 And), e1, e2) }
  | LPAREN; I32_OR; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S32 Or), e1, e2) }
  | LPAREN; I32_XOR; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S32 Xor), e1, e2) }
  | LPAREN; I32_MUL; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S32 Mul), e1, e2) }
  | LPAREN; I32_SHL; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S32 Shl), e1, e2) }
  | LPAREN; I32_SHR; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S32 Shr), e1, e2) }
  | LPAREN; I32_SHRU; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S32 ShrU), e1, e2) }
  | LPAREN; I32_REM; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S32 Rem), e1, e2) }
  | LPAREN; I32_REMU; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S32 RemU), e1, e2) }
  | LPAREN; I32_ROTL; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S32 Rotl), e1, e2) }
  | LPAREN; I32_ROTR; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S32 Rotr), e1, e2) }
  | LPAREN; BV64_TYPE; n = NUM; RPAREN; { Val (Bv (S64 (Int64.of_int n))) }
  | LPAREN; I64_NOT; ~ = bvexpr; RPAREN; { Unop (Bv (S64 Not), bvexpr) }
  | LPAREN; I64_NEG; ~ = bvexpr; RPAREN; { Unop (Bv (S64 Neg), bvexpr) }
  | LPAREN; I64_ADD; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S64 Add), e1, e2) }
  | LPAREN; I64_SUB; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S64 Sub), e1, e2) }
  | LPAREN; I64_DIV; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S64 Div), e1, e2) }
  | LPAREN; I64_DIVU; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S64 DivU), e1, e2) }
  | LPAREN; I64_AND; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S64 And), e1, e2) }
  | LPAREN; I64_OR; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S64 Or), e1, e2) }
  | LPAREN; I64_XOR; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S64 Xor), e1, e2) }
  | LPAREN; I64_MUL; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S64 Mul), e1, e2) }
  | LPAREN; I64_SHL; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S64 Shl), e1, e2) }
  | LPAREN; I64_SHR; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S64 Shr), e1, e2) }
  | LPAREN; I64_SHRU; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S64 ShrU), e1, e2) }
  | LPAREN; I64_REM; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S64 Rem), e1, e2) }
  | LPAREN; I64_REMU; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S64 RemU), e1, e2) }
  | LPAREN; I64_ROTL; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S64 Rotl), e1, e2) }
  | LPAREN; I64_ROTR; e1 = bvexpr; e2 = bvexpr; RPAREN; { Binop (Bv (S64 Rotr), e1, e2) }

let fpexpr :=
  | LPAREN; FP32_TYPE; n = DEC; RPAREN; { Val (Fp (S32 (Int32.bits_of_float n))) }
  | LPAREN; F32_NEG; ~ = fpexpr; RPAREN; { Unop (Fp (S32 Neg), fpexpr) }
  | LPAREN; F32_ABS; ~ = fpexpr; RPAREN; { Unop (Fp (S32 Abs), fpexpr) }
  | LPAREN; F32_SQRT; ~ = fpexpr; RPAREN; { Unop (Fp (S32 Sqrt), fpexpr) }
  | LPAREN; F32_NEAREST; ~ = fpexpr; RPAREN; { Unop (Fp (S32 Nearest), fpexpr) }
  | LPAREN; F32_IS_NAN; ~ = fpexpr; RPAREN; { Unop (Fp (S32 Is_nan), fpexpr) }
  | LPAREN; F32_ADD; e1 = fpexpr; e2 = fpexpr; RPAREN; { Binop (Fp (S32 Add), e1, e2) }
  | LPAREN; F32_SUB; e1 = fpexpr; e2 = fpexpr; RPAREN; { Binop (Fp (S32 Sub), e1, e2) }
  | LPAREN; F32_DIV; e1 = fpexpr; e2 = fpexpr; RPAREN; { Binop (Fp (S32 Div), e1, e2) }
  | LPAREN; F32_MUL; e1 = fpexpr; e2 = fpexpr; RPAREN; { Binop (Fp (S32 Mul), e1, e2) }
  | LPAREN; F32_REM; e1 = fpexpr; e2 = fpexpr; RPAREN; { Binop (Fp (S32 Rem), e1, e2) }
  | LPAREN; F32_MIN; e1 = fpexpr; e2 = fpexpr; RPAREN; { Binop (Fp (S32 Min), e1, e2) }
  | LPAREN; F32_MAX; e1 = fpexpr; e2 = fpexpr; RPAREN; { Binop (Fp (S32 Max), e1, e2) }
  | LPAREN; FP64_TYPE; n = DEC; RPAREN; { Val (Fp (S64 (Int64.bits_of_float n))) }
  | LPAREN; F64_NEG; ~ = fpexpr; RPAREN; { Unop (Fp (S64 Neg), fpexpr) }
  | LPAREN; F64_ABS; ~ = fpexpr; RPAREN; { Unop (Fp (S64 Abs), fpexpr) }
  | LPAREN; F64_SQRT; ~ = fpexpr; RPAREN; { Unop (Fp (S64 Sqrt), fpexpr) }
  | LPAREN; F64_NEAREST; ~ = fpexpr; RPAREN; { Unop (Fp (S64 Nearest), fpexpr) }
  | LPAREN; F64_IS_NAN; ~ = fpexpr; RPAREN; { Unop (Fp (S64 Is_nan), fpexpr) }
  | LPAREN; F64_ADD; e1 = fpexpr; e2 = fpexpr; RPAREN; { Binop (Fp (S64 Add), e1, e2) }
  | LPAREN; F64_SUB; e1 = fpexpr; e2 = fpexpr; RPAREN; { Binop (Fp (S64 Sub), e1, e2) }
  | LPAREN; F64_DIV; e1 = fpexpr; e2 = fpexpr; RPAREN; { Binop (Fp (S64 Div), e1, e2) }
  | LPAREN; F64_MUL; e1 = fpexpr; e2 = fpexpr; RPAREN; { Binop (Fp (S64 Mul), e1, e2) }
  | LPAREN; F64_REM; e1 = fpexpr; e2 = fpexpr; RPAREN; { Binop (Fp (S64 Rem), e1, e2) }
  | LPAREN; F64_MIN; e1 = fpexpr; e2 = fpexpr; RPAREN; { Binop (Fp (S64 Min), e1, e2) }
  | LPAREN; F64_MAX; e1 = fpexpr; e2 = fpexpr; RPAREN; { Binop (Fp (S64 Max), e1, e2) }

let bexpr :=
  | v = BOOL; { Val (Bool v) }
  | x = SYMBOL; { Symbol (mk_symbol Type.BoolTy x) }
  | LPAREN; BOOL_NOT; e = bexpr; RPAREN; { Unop (Bool Not, e) }
  | LPAREN; BOOL_AND; e1 = bexpr; e2 = bexpr; RPAREN; { Binop (Bool And, e1, e2) }
  | LPAREN; BOOL_XOR; e1 = bexpr; e2 = bexpr; RPAREN; { Binop (Bool Xor, e1, e2) }
  | LPAREN; BOOL_OR; e1 = bexpr; e2 = bexpr; RPAREN; { Binop (Bool Or, e1, e2) }
  | LPAREN; BOOL_EQ; e1 = bexpr; e2 = bexpr; RPAREN; { Relop (Bool Eq, e1, e2) }
  | LPAREN; BOOL_NE; e1 = bexpr; e2 = bexpr; RPAREN; { Relop (Bool Eq, e1, e2) }
  | LPAREN; BOOL_ITE; e1 = bexpr; e2 = bexpr; e3 = bexpr; RPAREN;
    { Triop (Bool Ite, e1, e2, e3) }
  | LPAREN; INT_EQ; e1 = iexpr; e2 = iexpr; RPAREN; { Relop (Int Eq, e1, e2) }
  | LPAREN; INT_NE; e1 = iexpr; e2 = iexpr; RPAREN; { Relop (Int Ne, e1, e2) }
  | LPAREN; INT_LT; e1 = iexpr; e2 = iexpr; RPAREN; { Relop (Int Lt, e1, e2) }
  | LPAREN; INT_LE; e1 = iexpr; e2 = iexpr; RPAREN; { Relop (Int Le, e1, e2) }
  | LPAREN; INT_GT; e1 = iexpr; e2 = iexpr; RPAREN; { Relop (Int Gt, e1, e2) }
  | LPAREN; INT_GE; e1 = iexpr; e2 = iexpr; RPAREN; { Relop (Int Ge, e1, e2) }
  | LPAREN; REAL_EQ; e1 = fexpr; e2 = fexpr; RPAREN; { Relop (Flt Eq, e1, e2) }
  | LPAREN; REAL_NE; e1 = fexpr; e2 = fexpr; RPAREN; { Relop (Flt Ne, e1, e2) }
  | LPAREN; REAL_LT; e1 = fexpr; e2 = fexpr; RPAREN; { Relop (Flt Lt, e1, e2) }
  | LPAREN; REAL_LE; e1 = fexpr; e2 = fexpr; RPAREN; { Relop (Flt Le, e1, e2) }
  | LPAREN; REAL_GT; e1 = fexpr; e2 = fexpr; RPAREN; { Relop (Flt Gt, e1, e2) }
  | LPAREN; REAL_GE; e1 = fexpr; e2 = fexpr; RPAREN; { Relop (Flt Ge, e1, e2) }
  | LPAREN; STR_EQ; e1 = sexpr; e2 = sexpr; RPAREN; { Relop (Str Eq, e1, e2) }
  | LPAREN; STR_NE; e1 = sexpr; e2 = sexpr; RPAREN; { Relop (Str Ne, e1, e2) }
  | LPAREN; I32_EQ; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S32 Eq), e1, e2) }
  | LPAREN; I32_NE; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S32 Ne), e1, e2) }
  | LPAREN; I32_LT; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S32 Lt), e1, e2) }
  | LPAREN; I32_LE; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S32 Le), e1, e2) }
  | LPAREN; I32_GT; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S32 Gt), e1, e2) }
  | LPAREN; I32_GE; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S32 Ge), e1, e2) }
  | LPAREN; I32_LTU; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S32 LtU), e1, e2) }
  | LPAREN; I32_LEU; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S32 LeU), e1, e2) }
  | LPAREN; I32_GTU; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S32 GtU), e1, e2) }
  | LPAREN; I32_GEU; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S32 GeU), e1, e2) }
  | LPAREN; I64_EQ; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S64 Eq), e1, e2) }
  | LPAREN; I64_NE; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S64 Ne), e1, e2) }
  | LPAREN; I64_LT; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S64 Lt), e1, e2) }
  | LPAREN; I64_LE; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S64 Le), e1, e2) }
  | LPAREN; I64_GT; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S64 Gt), e1, e2) }
  | LPAREN; I64_GE; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S64 Ge), e1, e2) }
  | LPAREN; I64_LTU; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S64 LtU), e1, e2) }
  | LPAREN; I64_LEU; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S64 LeU), e1, e2) }
  | LPAREN; I64_GTU; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S64 GtU), e1, e2) }
  | LPAREN; I64_GEU; e1 = bvexpr; e2 = bvexpr; RPAREN; { Relop (Bv (S64 GeU), e1, e2) }
  | LPAREN; F32_EQ; e1 = fpexpr; e2 = fpexpr; RPAREN; { Relop (Fp (S32 Eq), e1, e2) }
  | LPAREN; F32_NE; e1 = fpexpr; e2 = fpexpr; RPAREN; { Relop (Fp (S32 Ne), e1, e2) }
  | LPAREN; F32_LT; e1 = fpexpr; e2 = fpexpr; RPAREN; { Relop (Fp (S32 Lt), e1, e2) }
  | LPAREN; F32_LE; e1 = fpexpr; e2 = fpexpr; RPAREN; { Relop (Fp (S32 Le), e1, e2) }
  | LPAREN; F32_GT; e1 = fpexpr; e2 = fpexpr; RPAREN; { Relop (Fp (S32 Gt), e1, e2) }
  | LPAREN; F32_GE; e1 = fpexpr; e2 = fpexpr; RPAREN; { Relop (Fp (S32 Ge), e1, e2) }
  | LPAREN; F64_EQ; e1 = fpexpr; e2 = fpexpr; RPAREN; { Relop (Fp (S64 Eq), e1, e2) }
  | LPAREN; F64_NE; e1 = fpexpr; e2 = fpexpr; RPAREN; { Relop (Fp (S64 Ne), e1, e2) }
  | LPAREN; F64_LT; e1 = fpexpr; e2 = fpexpr; RPAREN; { Relop (Fp (S64 Lt), e1, e2) }
  | LPAREN; F64_LE; e1 = fpexpr; e2 = fpexpr; RPAREN; { Relop (Fp (S64 Le), e1, e2) }
  | LPAREN; F64_GT; e1 = fpexpr; e2 = fpexpr; RPAREN; { Relop (Fp (S64 Gt), e1, e2) }
  | LPAREN; F64_GE; e1 = fpexpr; e2 = fpexpr; RPAREN; { Relop (Fp (S64 Ge), e1, e2) }
