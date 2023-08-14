%{
open Core
open Expr

exception Invalid_type of string

let varmap : (string, string) Hashtbl.t = Hashtbl.create (module String)

let add_bind x t = Hashtbl.set varmap ~key:x ~data:t
let get_bind x = Hashtbl.find varmap x

let validate_bind (type a) (ty : a Type.ty) x : unit =
  let ty = Type.Pp.pp_ty ty in
  match get_bind x with
  | None -> add_bind x ty
  | Some ty' ->
    if not (String.equal ty ty') then
      let msg =
        sprintf "Const '%s' of type '%s' cannot be used as '%s'!" x ty' ty
      in
      raise (Invalid_type msg)

let mk_symbol (type a) (ty : a Type.ty) x : a Symbol.t =
  validate_bind ty x;
  match ty with
  | Type.IntTy -> Symbol.mk_symbol_int x
  | Type.RealTy -> Symbol.mk_symbol_real x
  | Type.BoolTy -> Symbol.mk_symbol_bool x
  | Type.StrTy -> Symbol.mk_symbol_str x
  | Type.NumTy -> assert false

%}
%token LPAREN
%token RPAREN
%token INT_NEG INT_ADD INT_SUB INT_DIV INT_MUL INT_REM
%token INT_EQ INT_NE INT_LT INT_LE INT_GT INT_GE
%token BOOL_NOT BOOL_AND BOOL_OR BOOL_XOR BOOL_EQ BOOL_NE BOOL_ITE
%token REAL_EQ REAL_NE
%token STR_EQ STR_NE
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

let sexpr :=
  | v = STR; { Val (Value.Str v) }
  | x = SYMBOL; { Symbol (mk_symbol Type.StrTy x) }

let bexpr :=
  | v = BOOL; { Val (Value.Bool v) }
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
  | LPAREN; STR_EQ; e1 = sexpr; e2 = sexpr; RPAREN; { Relop (Str Eq, e1, e2) }
  | LPAREN; STR_NE; e1 = sexpr; e2 = sexpr; RPAREN; { Relop (Str Ne, e1, e2) }

(* spec_constant : *)
(*   | NUM { Ast.Int $1 } *)
(*   | DEC { Ast.Real $1 } *)
(*   | STR { Ast.Str $1 } *)
(*   | BOOL { Ast.Bool $1 } *)
(*   | LPAREN; TYPE; NUM; RPAREN *)
(*     { *)
(*       match $2 with *)
(*       | `I32Type -> Ast.Num (I32 (Int32.of_int_trunc $3)) *)
(*       | `I64Type -> Ast.Num (I64 (Int64.of_int $3)) *)
(*       | _ -> failwith "invalid integer type" *)
(*     } *)
(*   | LPAREN; TYPE; DEC; RPAREN *)
(*     { *)
(*       match $2 with *)
(*       | `F32Type -> Ast.Num (F32 (Int32.bits_of_float $3)) *)
(*       | `F64Type -> Ast.Num (F64 (Int64.bits_of_float $3)) *)
(*       | _ -> failwith "invalid integer type" *)
(*     } *)
(*   ; *)
