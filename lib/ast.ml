open Core

(* type value = *)
(*   | Int of int *)
(*   | Real of float *)
(*   | Bool of bool *)
(*   | Num of Num.t *)
(*   | Str of string *)

(* type expr = *)
(*   | Val of value *)
(*   | Unop of unop * expr *)
(*   | Binop of binop * expr * expr *)
(*   | Relop of relop * expr * expr *)
(*   | Cvtop of cvtop * expr *)
(*   | Triop of triop * expr * expr * expr *)
(*   | Symbol of Symbol.t *)

type t =
  | Declare of Symbol.symbol
  | Assert of bool Expr.t
  | CheckSat
  | GetModel

(* let ( let* ) o f = Option.bind o ~f *)
(* let ( let+ ) o f = Option.map o ~f *)
(* let return x = Option.some x *)

(* let type_check_value (type a) (v : value) (ty : a Expr.ty) : a Value.t option = *)
(*   match (v, ty) with *)
(*   | Int x, IntTy -> Some (Value.Int x) *)
(*   | Real x, RealTy -> Some (Value.Real x) *)
(*   | Bool x, BoolTy -> Some (Value.Bool x) *)
(*   | Num x, NumTy -> Some (Value.Num x) *)
(*   | Str x, StrTy -> Some (Value.Str x) *)
(*   | _ -> None *)

(* let rec type_check_expr : 'a. expr -> 'a Expr.ty -> 'a Expr.t option = *)
(*   fun (type a) (e : expr) (ty : a Expr.ty) -> *)
(*    match (e, ty) with *)
(*    | Val x, ty -> *)
(*      let+ v = type_check_value x ty in *)
(*      Expr.Val v *)
(*    | Symbol x, _ty -> Some (Expr.Symbol x) *)
(*    | Unop (op, e), ty -> *)
(*      let+ e' = type_check_expr e ty in *)
(*      Expr.Unop (op, e') *)
(*    | Binop (op, e1, e2), ty -> *)
(*      let* e1' = type_check_expr e1 ty in *)
(*      let+ e2' = type_check_expr e2 ty in *)
(*      Expr.Binop (op, e1', e2') *)
(*    | Relop (op, e1, e2), BoolTy -> ( *)
(*      match (type_check_expr e1 NumTy, type_check_expr e2 NumTy) with *)
(*      | Some e1', Some e2' -> return (Expr.Relop (op, e1', e2')) *)
(*      | _ -> *)
(*        let* e1' = type_check_expr e1 BoolTy in *)
(*        let+ e2' = type_check_expr e2 BoolTy in *)
(*        Expr.Relop (op, e1', e2') ) *)
(*    | Cvtop (I32 ToBool, e), BoolTy -> *)
(*      let+ e' = type_check_expr e NumTy in *)
(*      Expr.Cvtop (I32 ToBool, e') *)
(*    | Cvtop (I32 OfBool, e), NumTy -> *)
(*      let+ e' = type_check_expr e BoolTy in *)
(*      Expr.Cvtop (I32 OfBool, e') *)
(*    | Cvtop (op, e), ty -> *)
(*      let+ e' = type_check_expr e ty in *)
(*      Expr.Cvtop (op, e') *)
(*    | Triop (op, e1, e2, e3), ty -> *)
(*      let* e1' = type_check_expr e1 ty in *)
(*      let* e2' = type_check_expr e2 ty in *)
(*      let+ e3' = type_check_expr e3 ty in *)
(*      Expr.Triop (op, e1', e2', e3') *)
(*    | _ -> None *)

let to_string (instr : t) : string =
  match instr with
  | Declare (Sym s) ->
    let symb = Symbol.Pp.pp s
    and t = Type.Pp.pp_ty (Symbol.type_of s) in
    sprintf "(declare-fun %s %s)" symb t
  | Assert e -> sprintf "(assert %s)" (Expr.Pp.pp e)
  | CheckSat -> "(check-sat)"
  | GetModel -> "(get-model)"
