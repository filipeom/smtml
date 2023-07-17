open Core
open Types
module Expr = Expression

type value =
  | Int of int
  | Real of float
  | Bool of bool
  | Num of Num.t
  | Str of string

type expr =
  | Val of value
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Relop of relop * expr * expr
  | Cvtop of cvtop * expr
  | Triop of triop * expr * expr * expr
  | Symbol of Symbol.t

type t =
  | Declare of Symbol.t
  | Assert of expr
  | CheckSat
  | GetModel

let ( let* ) o f = Option.bind o ~f
let ( let+ ) o f = Option.map o ~f
let return x = Option.some x

let type_check_value (type a) (v : value) (ty : a Expr.ty) : a Value.t option =
  match (v, ty) with
  | Int x, IntTy -> Some (Value.Int x)
  | Real x, RealTy -> Some (Value.Real x)
  | Bool x, BoolTy -> Some (Value.Bool x)
  | Num x, NumTy -> Some (Value.Num x)
  | Str x, StrTy -> Some (Value.Str x)
  | _ -> None

(*
let ty_of_type : type a. expr_type -> a Expr.ty = function
  | `IntType -> IntTy
  | `RealType -> RealTy
  | _ -> assert false
  *)

let rec type_check_expr : 'a. expr -> 'a Expr.ty -> 'a Expr.t option =
  fun (type a) (e : expr) (ty : a Expr.ty) ->
   match (e, ty) with
   | Val x, ty ->
     let+ v = type_check_value x ty in
     Expr.Val v
   | Symbol x, _ty -> Some (Expr.Symbol x)
   | Unop (op, e), ty ->
     let+ e' = type_check_expr e ty in
     Expr.Unop (op, e')
   | Binop (op, e1, e2), ty ->
     let* e1' = type_check_expr e1 ty in
     let+ e2' = type_check_expr e2 ty in
     Expr.Binop (op, e1', e2')
   | Relop (op, e1, e2), BoolTy -> (
     match (type_check_expr e1 NumTy, type_check_expr e2 NumTy) with
     | Some e1', Some e2' -> return (Expr.Relop (op, e1', e2'))
     | _ ->
       let* e1' = type_check_expr e1 BoolTy in
       let+ e2' = type_check_expr e2 BoolTy in
       Expr.Relop (op, e1', e2') )
   | Cvtop (I32 ToBool, e), BoolTy ->
     let+ e' = type_check_expr e NumTy in
     Expr.Cvtop (I32 ToBool, e')
   | Cvtop (I32 OfBool, e), NumTy ->
     let+ e' = type_check_expr e BoolTy in
     Expr.Cvtop (I32 OfBool, e')
   | Cvtop (op, e), ty ->
     let+ e' = type_check_expr e ty in
     Expr.Cvtop (op, e')
   | Triop (op, e1, e2, e3), ty ->
     let* e1' = type_check_expr e1 ty in
     let* e2' = type_check_expr e2 ty in
     let+ e3' = type_check_expr e3 ty in
     Expr.Triop (op, e1', e2', e3')
   | _ -> None

let string_of_uvalue = function
  | Int x -> Int.to_string x
  | Real x -> Float.to_string x
  | Bool x -> Bool.to_string x
  | Num x -> Num.to_string x
  | Str x -> sprintf "\"%s\"" x

let rec string_of_expr = function
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
    sprintf "(%s %s)" str_op (string_of_expr e)
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
    sprintf "(%s %s %s)" str_op (string_of_expr e1) (string_of_expr e2)
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
    sprintf "(%s %s %s %s)" str_op (string_of_expr e1) (string_of_expr e2)
      (string_of_expr e3)
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
    sprintf "(%s %s %s)" str_op (string_of_expr e1) (string_of_expr e2)
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
    sprintf "(%s %s)" str_op (string_of_expr e)
  | Symbol s -> Symbol.to_string s

let to_string (instr : t) : string =
  match instr with
  | Declare s ->
    let symb = Symbol.to_string s
    and t = Types.string_of_type (Symbol.type_of s) in
    sprintf "(declare-fun %s %s)" symb t
  | Assert e -> sprintf "(assert %s)" (string_of_expr e)
  | CheckSat -> "(check-sat)"
  | GetModel -> "(get-model)"
