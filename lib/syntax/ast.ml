open Core
open Types

type uvalue =
  | Int of int
  | Real of float
  | Bool of bool
  | Num of Num.t
  | Str of string

type uexpr =
  | Val of uvalue
  | Unop of unop * uexpr
  | Binop of binop * uexpr * uexpr
  | Relop of relop * uexpr * uexpr
  | Cvtop of cvtop * uexpr
  | Triop of triop * uexpr * uexpr * uexpr
  | Symbol of Symbol.t

type t =
  | Declare of Symbol.t
  | Assert of uexpr
  | CheckSat
  | GetModel

let string_of_uvalue = function
  | Int x -> Int.to_string x
  | Real x -> Float.to_string x
  | Bool x -> Bool.to_string x
  | Num x -> Num.to_string x
  | Str x -> sprintf "\"%s\"" x

let rec string_of_uexpr = function
  | Val v -> string_of_uvalue v
  | Unop (op, e) ->
    let str_op =
      match op with
      | Int op -> "int." ^ I.string_of_unop op
      | Real op -> "real." ^ R.string_of_unop op
      | Bool op -> "bool." ^ B.string_of_unop op
      | Str op -> "str." ^ S.string_of_unop op
      | I32 op -> "i32." ^ I32.string_of_unop op
      | I64 op -> "i64." ^ I64.string_of_unop op
      | F32 op -> "f32." ^ F32.string_of_unop op
      | F64 op -> "f64." ^ F64.string_of_unop op
    in
    sprintf "(%s %s)" str_op (string_of_uexpr e)
  | Binop (op, e1, e2) ->
    let str_op =
      match op with
      | Int op -> "int." ^ I.string_of_binop op
      | Real op -> "real." ^ R.string_of_binop op
      | Bool op -> "bool." ^ B.string_of_binop op
      | Str op -> "str." ^ S.string_of_binop op
      | I32 op -> "i32." ^ I32.string_of_binop op
      | I64 op -> "i64." ^ I64.string_of_binop op
      | F32 op -> "f32." ^ F32.string_of_binop op
      | F64 op -> "f64." ^ F64.string_of_binop op
    in
    sprintf "(%s %s %s)" str_op (string_of_uexpr e1) (string_of_uexpr e2)
  | Triop (op, e1, e2, e3) ->
    let str_op =
      match op with
      | Int op -> "int." ^ I.string_of_triop op
      | Real op -> "real." ^ R.string_of_triop op
      | Bool op -> "bool." ^ B.string_of_triop op
      | Str op -> "str." ^ S.string_of_triop op
      | I32 op -> "i32." ^ I32.string_of_triop op
      | I64 op -> "i64." ^ I64.string_of_triop op
      | F32 op -> "f32." ^ F32.string_of_triop op
      | F64 op -> "f64." ^ F64.string_of_triop op
    in
    sprintf "(%s %s %s %s)" str_op (string_of_uexpr e1) (string_of_uexpr e2)
      (string_of_uexpr e3)
  | Relop (op, e1, e2) ->
    let str_op =
      match op with
      | Int op -> "int." ^ I.string_of_relop op
      | Real op -> "real." ^ R.string_of_relop op
      | Bool op -> "bool." ^ B.string_of_relop op
      | Str op -> "str." ^ S.string_of_relop op
      | I32 op -> "i32." ^ I32.string_of_relop op
      | I64 op -> "i64." ^ I64.string_of_relop op
      | F32 op -> "f32." ^ F32.string_of_relop op
      | F64 op -> "f64." ^ F64.string_of_relop op
    in
    sprintf "(%s %s %s)" str_op (string_of_uexpr e1) (string_of_uexpr e2)
  | Cvtop (op, e) ->
    let str_op =
      match op with
      | Int op -> "int." ^ I.string_of_cvtop op
      | Real op -> "real." ^ R.string_of_cvtop op
      | Bool op -> "bool." ^ B.string_of_cvtop op
      | Str op -> "str." ^ S.string_of_cvtop op
      | I32 op -> "i32." ^ I32.string_of_cvtop op
      | I64 op -> "i64." ^ I64.string_of_cvtop op
      | F32 op -> "f32." ^ F32.string_of_cvtop op
      | F64 op -> "f64." ^ F64.string_of_cvtop op
    in
    sprintf "(%s %s)" str_op (string_of_uexpr e)
  | Symbol s -> Symbol.to_string s

let to_string (instr : t) : string =
  match instr with
  | Declare s ->
    let symb = Symbol.to_string s
    and t = Types.string_of_type (Symbol.type_of s) in
    sprintf "(declare-fun %s %s)" symb t
  | Assert e -> sprintf "(assert %s)" (string_of_uexpr e)
  | CheckSat -> "(check-sat)"
  | GetModel -> "(get-model)"
