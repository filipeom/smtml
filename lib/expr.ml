open Core
open Type

exception InvalidRelop

(* type qt = *)
(*   | Forall *)
(*   | Exists *)

type _ t =
  | Val : 'a Value.t -> 'a t
  | SymPtr : int32 * 'a t -> 'a t
  | Unop : 'a unop * 'b t -> 'a t
  | Binop : 'a binop * 'a t * 'a t -> 'a t
  | Relop : 'a relop * 'a t * 'a t -> bool t
  | Cvtop : ('a, 'b) cvtop * 'a t -> 'b t
  | Triop : 'a triop * 'a t * 'b t * 'b t -> 'a t
  | Symbol : 'a Symbol.t -> 'a t
  | Extract : 'a t * int * int -> 'a t
  | Concat : 'a t * 'a t -> 'a t
(* | Quantifier of qt * Symbol.t list * expr * expr list list *)

type expr = E : 'a t -> expr

let mk_symbol_int (x : string) : int t = Symbol (Symbol.mk_symbol_int x)
let mk_symbol_real (x : string) : float t = Symbol (Symbol.mk_symbol_real x)
let mk_symbol_bool (x : string) : bool t = Symbol (Symbol.mk_symbol_bool x)
let mk_symbol_str (x : string) : string t = Symbol (Symbol.mk_symbol_str x)
let mk_symbol_num (x : string) : Num.t t = Symbol (Symbol.mk_symbol_num x)

let ( ++ ) (e1 : _ t) (e2 : _ t) = Concat (e1, e2)
let is_num (e : _ t) : bool = match e with Val (Num _) -> true | _ -> false
let is_val (e : _ t) : bool = match e with Val _ -> true | _ -> false
let is_unop (e : _ t) : bool = match e with Unop _ -> true | _ -> false
let is_relop (e : _ t) : bool = match e with Relop _ -> true | _ -> false
let is_binop (e : _ t) : bool = match e with Binop _ -> true | _ -> false
let is_cvtop (e : _ t) : bool = match e with Cvtop _ -> true | _ -> false
let is_triop (e : _ t) : bool = match e with Triop _ -> true | _ -> false

let is_concrete (e : _ t) : bool =
  match e with Val _ | SymPtr (_, Val _) -> true | _ -> false

(* let rec equal : 'a. 'a t -> 'a t -> bool = *)
(*   fun (type a) (e1 : a t) (e2 : a t) -> *)
(*    match (e1, e2) with *)
(*    | Val v1, Val v2 -> Value.equal v1 v2 *)
(*    | SymPtr (b1, o1), SymPtr (b2, o2) -> Int32.(b1 = b2) && equal o1 o2 *)
(*    | Unop (op1, _e1), Unop (op2, _e2) -> Poly.(op1 = op2) && assert false *)
(*    | Cvtop (op1, _e1), Cvtop (op2, _e2) -> Poly.(op1 = op2) && assert false *)
(*    | Binop (op1, e1, e3), Binop (op2, e2, e4) -> *)
(*      Poly.(op1 = op2) && equal e1 e2 && equal e3 e4 *)
(*    | Relop (op1, _e1, _e3), Relop (op2, _e2, _e4) -> *)
(*      Poly.(op1 = op2) && assert false *)
(*    | Triop (op1, e1, _e3, _e5), Triop (op2, e2, _e4, _e6) -> *)
(*      Poly.(op1 = op2) && equal e1 e2 && assert false *)
(*    | Symbol s1, Symbol s2 -> Symbol.equal s1 s2 *)
(*    | Extract (e1, h1, l1), Extract (e2, h2, l2) -> *)
(*      equal e1 e2 && Int.(h1 = h2) && Int.(l1 = l2) *)
(*    | Concat (e1, e3), Concat (e2, e4) -> equal e1 e2 && equal e3 e4 *)
(*    | Quantifier (q1, vars1, e1, p1), Quantifier (q2, vars2, e2, p2) -> *)
(*       Poly.(q1 = q2) *)
(*       && List.equal Symbol.equal vars1 vars2 *)
(*       && equal e1 e2 *)
(*       && List.equal (List.equal equal) p1 p2 *)
(*    | _ -> false *)

let rec length : type a. a t -> int = function
  | Val _ -> 1
  | SymPtr _ -> 1
  | Unop (_, e) -> 1 + length e
  | Binop (_, e1, e2) -> 1 + length e1 + length e2
  | Triop (_, e1, e2, e3) -> 1 + length e1 + length e2 + length e3
  | Relop (_, e1, e2) -> 1 + length e1 + length e2
  | Cvtop (_, e) -> 1 + length e
  | Symbol _ -> 1
  | Extract (e, _, _) -> 1 + length e
  | Concat (e1, e2) -> 1 + length e1 + length e2
(*  | Quantifier (_, _, body, _) -> length body*)

(* let get_symbols (e : _ t list) : _ Symbol.t list = *)
(*   let rec symbols : type a. a t -> a Symbol.t list = function *)
(*     | Val _ -> [] *)
(*     | SymPtr (_, offset) -> symbols offset *)
(*     | Unop (_, e1) -> symbols e1 *)
(*     | Binop (_, e1, e2) -> symbols e1 @ symbols e2 *)
(*     | Triop (_, e1, e2, e3) -> symbols e1 @ symbols e2 @ symbols e3 *)
(*     | Relop (_, e1, e2) -> symbols e1 @ symbols e2 *)
(*     | Cvtop (_, e) -> symbols e *)
(*     | Symbol s -> [ s ] *)
(*     | Extract (e, _, _) -> symbols e *)
(*     | Concat (e1, e2) -> symbols e1 @ symbols e2 *)
(*   in *)
(*   List.fold (List.concat_map e ~f:symbols) ~init:[] ~f:(fun accum x -> *)
(*     if List.mem accum x ~equal:Symbol.equal then accum else x :: accum ) *)

let rename_symbols (es : _ t list) : _ t list =
  let count = ref 0
  and map = Hashtbl.create (module String) in
  let rec rename : type a. a t -> a t = function
    | Val _ as e -> e
    | SymPtr (i, offset) -> SymPtr (i, rename offset)
    | Unop (op, e) -> Unop (op, rename e)
    | Binop (op, e1, e2) -> Binop (op, rename e1, rename e2)
    | Triop (op, e1, e2, e3) -> Triop (op, rename e1, rename e2, rename e3)
    | Relop (op, e1, e2) -> Relop (op, rename e1, rename e2)
    | Cvtop (op, e) -> Cvtop (op, rename e)
    | Symbol s ->
      let old_name = Symbol.Pp.pp s in
      let new_name =
        if Hashtbl.mem map old_name then Hashtbl.find_exn map old_name
        else
          let x = "x" ^ Int.to_string !count in
          Hashtbl.set map ~key:old_name ~data:x;
          count := !count + 1;
          x
      in
      Symbol (Symbol.rename s new_name)
    | Extract (e, h, l) -> Extract (rename e, h, l)
    | Concat (e1, e2) -> Concat (rename e1, rename e2)
  in
  List.map ~f:rename es

module Integer = struct
  let const (i : int) : int t = Val (Int i)
  let neg (e : int t) : int t = Unop (Int Neg, e)
  let ( + ) (e1 : int t) (e2 : int t) : int t = Binop (Int Add, e1, e2)
  let ( - ) (e1 : int t) (e2 : int t) : int t = Binop (Int Sub, e1, e2)
  let ( * ) (e1 : int t) (e2 : int t) : int t = Binop (Int Mul, e1, e2)
  let ( / ) (e1 : int t) (e2 : int t) : int t = Binop (Int Div, e1, e2)
  let rem (e1 : int t) (e2 : int t) : int t = Binop (Int Rem, e1, e2)
  let ( = ) (e1 : int t) (e2 : int t) : bool t = Relop (Int Eq, e1, e2)
  let ( != ) (e1 : int t) (e2 : int t) : bool t = Relop (Int Ne, e1, e2)
  let ( < ) (e1 : int t) (e2 : int t) : bool t = Relop (Int Lt, e1, e2)
  let ( <= ) (e1 : int t) (e2 : int t) : bool t = Relop (Int Le, e1, e2)
  let ( > ) (e1 : int t) (e2 : int t) : bool t = Relop (Int Gt, e1, e2)
  let ( >= ) (e1 : int t) (e2 : int t) : bool t = Relop (Int Ge, e1, e2)
  let cvt_to_string (e : int t) : string t = Cvtop (Int ToString, e)
  let cvt_of_string (e : string t) : int t = Cvtop (Int OfString, e)
  let cvt_of_real (e : float t) : int t = Cvtop (Int ReinterpretFloat, e)
end

module Boolean = struct
  let const (b : bool) : bool t = Val (Bool b)
  let not_ (e : bool t) : bool t = Unop (Bool Not, e)
  let and_ (e1 : bool t) (e2 : bool t) : bool t = Binop (Bool And, e1, e2)
  let or_ (e1 : bool t) (e2 : bool t) : bool t = Binop (Bool Or, e1, e2)
  let xor (e1 : bool t) (e2 : bool t) : bool t = Binop (Bool Xor, e1, e2)
  let eq (e1 : bool t) (e2 : bool t) : bool t = Relop (Bool Eq, e1, e2)
  let ne (e1 : bool t) (e2 : bool t) : bool t = Relop (Bool Ne, e1, e2)

  let ite (e1 : bool t) (e2 : bool t) (e3 : bool t) =
    Triop (Bool Ite, e1, e2, e3)
end

module Pp = struct
  let rec pp : type a. a t -> string = function
    | Val v -> Value.Pp.pp v
    | SymPtr (base, offset) ->
      let str_o = pp offset in
      sprintf "(i32.add (i32 %ld) %s)" base str_o
    | Unop (op, e) -> sprintf "(%s %s)" (Pp.pp_unop op) (pp e)
    | Binop (op, e1, e2) ->
      sprintf "(%s %s %s)" (Pp.pp_binop op) (pp e1) (pp e2)
    | Triop (op, e1, e2, e3) ->
      sprintf "(%s %s %s %s)" (Pp.pp_triop op) (pp e1) (pp e2) (pp e3)
    | Relop (op, e1, e2) ->
      sprintf "(%s %s %s)" (Pp.pp_relop op) (pp e1) (pp e2)
    | Cvtop (op, e) -> sprintf "(%s %s)" (Pp.pp_cvtop op) (pp e)
    | Symbol s -> Symbol.Pp.pp s
    | Extract (e, h, l) -> sprintf "(extract %s %d %d)" (pp e) l h
    | Concat (e1, e2) -> sprintf "(++ %s %s)" (pp e1) (pp e2)
end

(* let type_of (type a) (e : a t) : expr_type = *)
(*   match e with *)
(*   | Val v -> Value.type_of v *)
(*   | SymPtr _ -> `I32Type *)
(*   | Binop (op, _, _) -> Types.type_of op *)
(*   | Triop (op, _, _, _) -> Types.type_of op *)
(*   | Unop (op, _) -> Types.type_of op *)
(*   | Relop (op, _, _) -> Types.type_of op *)
(*   | Cvtop (op, _) -> Types.type_of op *)
(*   | Symbol s -> Symbol.type_of s *)
(*   | Extract (_, _h, _l) -> assert false *)
(*   | Concat (_e1, _e2) -> assert false *)

(* let negate_relop (type a) (e : a t) : a t = *)
(*   match e with *)
(*   | Relop (op, e1, e2) -> ( *)
(*     match op with *)
(*     | Int op' -> Relop (Int (I.neg_relop op'), e1, e2) *)
(*     | Real op' -> Relop (Real (R.neg_relop op'), e1, e2) *)
(*     | Bool op' -> Relop (Bool (B.neg_relop op'), e1, e2) *)
(*     | Str op' -> Relop (Str (S.neg_relop op'), e1, e2) *)
(*     | I32 op' -> Relop (I32 (I32.neg_relop op'), e1, e2) *)
(*     | I64 op' -> Relop (I64 (I64.neg_relop op'), e1, e2) *)
(*     | F32 op' -> Relop (F32 (F32.neg_relop op'), e1, e2) *)
(*     | F64 op' -> Relop (F64 (F64.neg_relop op'), e1, e2) ) *)
(*   | _ -> raise InvalidRelop *)

(* let to_smt (es : _ t list) : string = *)
(*   let symbols = *)
(*     List.map (get_symbols es) ~f:(fun s -> *)
(*       let x = Symbol.to_string s *)
(*       and t = Symbol.type_of s in *)
(*       sprintf "(declare-fun %s %s)" x (Types.string_of_type t) ) *)
(*   in *)
(*   let es' = List.map es ~f:(fun e -> sprintf "(assert %s)" (to_string e)) in *)
(*   String.concat ~sep:"\n" (symbols @ es' @ [ "(check-sat)" ]) *)

(* let string_of_exprs (pc : _ t list) : string = *)
(*   let pc' = String.concat ~sep:" " (List.map ~f:to_string pc) in *)
(*   if List.length pc > 1 then sprintf "(and %s)" pc' else pc' *)

let rec get_ptr : type a. a t -> Num.t option = function
  (* FIXME: this function can be "simplified" *)
  | Val _ -> None
  | SymPtr (base, _) -> Some (I32 base)
  | Unop (_, e) -> get_ptr e
  | Binop (_, e1, e2) ->
    let p1 = get_ptr e1 in
    if Option.is_some p1 then p1 else get_ptr e2
  | Triop (_, e1, e2, e3) ->
    let p1 = get_ptr e1 in
    if Option.is_some p1 then p1
    else
      let p2 = get_ptr e2 in
      if Option.is_some p2 then p2 else get_ptr e3
  | Relop (_, e1, e2) ->
    let p1 = get_ptr e1 in
    if Option.is_some p1 then p1 else get_ptr e2
  | Cvtop (_, e) -> get_ptr e
  | Symbol _ -> None
  | Extract (e, _, _) -> get_ptr e
  | Concat (e1, e2) ->
    (* assume concatenation of only one ptr *)
    let p1 = get_ptr e1 in
    if Option.is_some p1 then p1 else get_ptr e2

let concretize_ptr (e : Num.t t) : Num.t option =
  (* TODO: this should work with symbolic pointers *)
  (* would probably introduce Memory Objects here *)
  let open Int32 in
  match e with
  | Val (Num n) -> Some n
  | SymPtr (base, Val (Num (I32 offset))) -> Some (I32 (base + offset))
  | _ -> None

let concretize_base_ptr (e : Num.t t) : int32 option =
  match e with SymPtr (base, _) -> Some base | _ -> None

(* let to_bool : type a. a t -> bool t option = function *)
(*   | Val _ | SymPtr _ -> None *)
(*   | (Relop _ as e') -> Some e' *)
(*   | Cvtop (I32 OfBool, _e') -> assert false *)
(*   | _ as e -> Some (Cvtop (I32 ToBool, e)) *)

let nland64 (x : int64) (n : int) =
  let rec loop x' n' acc =
    if n' = 0 then Int64.(x' land acc)
    else loop x' (n' - 1) Int64.(shift_left acc 8 lor 0xffL)
  in
  loop x n 0L

let nland32 (x : int32) (n : int) =
  let rec loop x' n' acc =
    if n' = 0 then Int32.(x' land acc)
    else loop x' (n' - 1) Int32.(shift_left acc 8 lor 0xffl)
  in
  loop x n 0l

(*
let rec simplify ?(extract = true) (e : _ t) : _ t =
  match e with
  | Val v -> Val v
  | SymPtr (base, offset) -> SymPtr (base, simplify offset)
  | Binop (I32 op, e1, e2) -> (
    let e1' = simplify e1
    and e2' = simplify e2 in
    match (e1', e2') with
    | SymPtr (b1, os1), SymPtr (b2, os2) -> (
      match op with
      | Sub when Int32.(b1 = b2) -> simplify (Binop (I32 Sub, os1, os2))
      | _ -> Binop (I32 op, e1', e2') )
    | SymPtr (base, offset), _ -> (
      match op with
      | Add ->
        let new_offset = simplify (Binop (I32 Add, offset, e2')) in
        simplify (SymPtr (base, new_offset))
      | Sub ->
        let new_offset = simplify (Binop (I32 Sub, offset, e2')) in
        simplify (SymPtr (base, new_offset))
      | _ -> Binop (I32 op, e1', e2') )
    | _, SymPtr (base, offset) -> (
      match op with
      | Add ->
        let new_offset = simplify (Binop (I32 Add, offset, e1')) in
        simplify (SymPtr (base, new_offset))
      | _ -> Binop (I32 op, e1', e2') )
    | Val (Num (I32 0l)), _ -> (
      match op with
      | Add | Or | Sub -> e2'
      | And | DivS | DivU | Mul | RemS | RemU -> Val (Num (I32 0l))
      | _ -> Binop (I32 op, e1', e2') )
    | _, Val (Num (I32 0l)) -> (
      match op with
      | Add | Or | Sub -> e1'
      | And | Mul -> Val (Num (I32 0l))
      | _ -> Binop (I32 op, e1', e2') )
    | Val (Num n1), Val (Num n2) ->
      Val (Num (Eval_numeric.eval_binop (I32 op) n1 n2))
    | Binop (I32 op2, x, Val (Num v1)), Val (Num v2) when not (is_num x) -> (
      match (op, op2) with
      | Add, Add ->
        let v = Eval_numeric.eval_binop (I32 Add) v1 v2 in
        Binop (I32 Add, x, Val (Num v))
      | Add, Sub | Sub, Add ->
        let v = Eval_numeric.eval_binop (I32 Sub) v1 v2 in
        Binop (I32 Add, x, Val (Num v))
      | Sub, Sub ->
        let v = Eval_numeric.eval_binop (I32 Add) v1 v2 in
        Binop (I32 Sub, x, Val (Num v))
      | _, _ -> Binop (I32 op, e1', e2') )
    | (bop, Val (Num (I32 1l)) | Val (Num (I32 1l)), bop)
      when is_relop bop && Poly.(op = And) ->
      bop
    | _ -> Binop (I32 op, e1', e2') )
  | Binop (I64 op, e1, e2) -> (
    let e1' = simplify e1
    and e2' = simplify e2 in
    match (e1', e2') with
    | SymPtr (b1, os1), SymPtr (b2, os2) -> (
      match op with
      | Sub when Int32.(b1 = b2) -> simplify (Binop (I64 Sub, os1, os2))
      | _ -> Binop (I64 op, e1', e2') )
    | SymPtr (base, offset), _ -> (
      match op with
      | Add ->
        let new_offset = simplify (Binop (I64 Add, offset, e2')) in
        simplify (SymPtr (base, new_offset))
      | Sub ->
        let new_offset = simplify (Binop (I64 Sub, offset, e2')) in
        simplify (SymPtr (base, new_offset))
      | _ -> Binop (I64 op, e1', e2') )
    | _, SymPtr (base, offset) -> (
      match op with
      | Add ->
        let new_offset = simplify (Binop (I64 Add, offset, e1')) in
        simplify (SymPtr (base, new_offset))
      | _ -> Binop (I64 op, e1', e2') )
    | Val (Num (I64 0L)), _ -> (
      match op with
      | Add | Or | Sub -> e2'
      | And | DivS | DivU | Mul | RemS | RemU -> Val (Num (I64 0L))
      | _ -> Binop (I64 op, e1', e2') )
    | _, Val (Num (I64 0L)) -> (
      match op with
      | Add | Or | Sub -> e1'
      | And | Mul -> Val (Num (I64 0L))
      | _ -> Binop (I64 op, e1', e2') )
    | Val (Num v1), Val (Num v2) ->
      Val (Num (Eval_numeric.eval_binop (I64 op) v1 v2))
    | Binop (I64 op2, x, Val (Num v1)), Val (Num v2) when not (is_num x) -> (
      match (op, op2) with
      | Add, Add ->
        let v = Eval_numeric.eval_binop (I64 Add) v1 v2 in
        Binop (I64 Add, x, Val (Num v))
      | Add, Sub | Sub, Add ->
        let v = Eval_numeric.eval_binop (I64 Sub) v1 v2 in
        Binop (I64 Add, x, Val (Num v))
      | Sub, Sub ->
        let v = Eval_numeric.eval_binop (I64 Add) v1 v2 in
        Binop (I64 Sub, x, Val (Num v))
      | _, _ -> Binop (I64 op, e1', e2') )
    | (bop, Val (Num (I64 1L)) | Val (Num (I64 1L)), bop)
      when is_relop bop && Poly.(op = And) ->
      bop
    | _ -> Binop (I64 op, e1', e2') )
  | Relop (I32 op, e1, e2) -> (
    let e1' = simplify e1
    and e2' = simplify e2 in
    match (e1', e2') with
    | Val (Num v1), Val (Num v2) ->
      let ret = Eval_numeric.eval_relop (I32 op) v1 v2 in
      Val (Num (Num.num_of_bool ret))
    | SymPtr (_, _), Val (Num (I32 0l)) | Val (Num (I32 0l)), SymPtr (_, _) -> (
      match op with
      | Eq -> Val (Num (I32 0l))
      | Ne -> Val (Num (I32 1l))
      | _ -> Relop (I32 op, e1', e2') )
    | SymPtr (b1, os1), SymPtr (b2, os2) -> (
      let open Int32 in
      match op with
      | Eq when b1 = b2 -> Relop (I32 Eq, os1, os2)
      | Eq when b1 <> b2 -> Val (Num (I32 0l))
      | Ne when b1 = b2 -> Relop (I32 Ne, os1, os2)
      | Ne when b1 <> b2 -> Val (Num (I32 1l))
      | LtU when b1 = b2 -> Relop (I32 LtU, os1, os2)
      | LeU when b1 = b2 -> Relop (I32 LeU, os1, os2)
      | LtU -> Relop (I32 LtU, Val (Num (I32 b1)), Val (Num (I32 b2)))
      | LeU -> Relop (I32 LeU, Val (Num (I32 b1)), Val (Num (I32 b2)))
      | GtU when b1 = b2 -> Relop (I32 GtU, os1, os2)
      | GeU when b1 = b2 -> Relop (I32 GeU, os1, os2)
      | GtU -> Relop (I32 GtU, Val (Num (I32 b1)), Val (Num (I32 b2)))
      | GeU -> Relop (I32 GeU, Val (Num (I32 b1)), Val (Num (I32 b2)))
      | _ -> Relop (I32 op, e1', e2') )
    | _ -> Relop (I32 op, e1', e2') )
  | Extract (_, _, _) when not extract -> e
  | Extract (s, h, l) when extract -> (
    match s with
    | Val (Num (I64 x)) ->
      let x' = nland64 (Int64.shift_right x (l * 8)) (h - l) in
      Val (Num (I64 x'))
    | _ when h - l = size (type_of s) -> s
    | _ -> e )
  | Concat (e1, e2) -> (
    let e1' = simplify ~extract:false e1
    and e2' = simplify ~extract:false e2 in
    match (e1', e2') with
    | Extract (Val (Num (I64 x2)), h2, l2), Extract (Val (Num (I64 x1)), h1, l1)
      ->
      let d1 = h1 - l1
      and d2 = h2 - l2 in
      let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1
      and x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
      let x = Int64.(shift_left x2' (Int.( * ) d1 8) lor x1') in
      Extract (Val (Num (I64 x)), d1 + d2, 0)
    | Extract (Val (Num (I32 x2)), h2, l2), Extract (Val (Num (I32 x1)), h1, l1)
      ->
      let d1 = h1 - l1
      and d2 = h2 - l2 in
      let x1' = nland32 (Int32.shift_right x1 (l1 * 8)) d1
      and x2' = nland32 (Int32.shift_right x2 (l2 * 8)) d2 in
      let x = Int32.(shift_left x2' (Int.( * ) d1 8) lor x1') in
      Extract (Val (Num (I32 x)), d1 + d2, 0)
    | Extract (s1, h, m1), Extract (s2, m2, l) when Poly.(s1 = s2) && m1 = m2 ->
      Extract (s1, h, l)
    | ( Extract (Val (Num (I64 x2)), h2, l2)
      , Concat (Extract (Val (Num (I64 x1)), h1, l1), se) )
      when not (is_num se) ->
      let d1 = h1 - l1
      and d2 = h2 - l2 in
      let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1
      and x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
      let x = Int64.(shift_left x2' (Int.( * ) d1 8) lor x1') in
      Extract (Val (Num (I64 x)), d1 + d2, 0) ++ se
    | _ -> e1' ++ e2' )
  | _ -> e
  *)

(* let mk_relop (e : Num.t t) (t : num_type) : bool t = *)
(*   let zero = Value.Num (Num.default_value t) in *)
(*   match t with *)
(*   | `I32Type -> Relop (I32 Ne, e, Val zero) *)
(*   | `I64Type -> Relop (I64 Ne, e, Val zero) *)
(*   | `F32Type -> Relop (F32 Ne, e, Val zero) *)
(*   | `F64Type -> Relop (F64 Ne, e, Val zero) *)

(* let add_constraint ?(neg : bool = false) (e : _ t) (pc : _ t) : _ t = *)
(*   let cond = *)
(*     let c = to_bool e in *)
(*     if neg then Option.map ~f:(fun e -> Unop (Bool Not, e)) c else c *)
(*   in *)
(*   Option.fold cond ~init:pc ~f:(fun pc c -> *)
(*     match pc with _ -> Binop (Bool And, c, pc) ) *)

(* let insert_pc ?(neg : bool = false) (e : _ t) (pc : _ t list) : _ t list = *)
(*   let cond = *)
(*     let c = to_bool e in *)
(*     if neg then Option.map ~f:(fun e -> Unop (Bool Not, e)) c else c *)
(*   in *)
(*   Option.fold cond ~init:pc ~f:(fun pc a -> a :: pc) *)