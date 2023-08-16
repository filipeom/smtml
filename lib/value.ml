module BV = struct
  type ('a, 'b) op =
    | S32 : 'a -> ('a, 'b) op
    | S64 : 'b -> ('a, 'b) op

  type t = (int32, int64) op

  let equal (v1 : t) (v2 : t) : bool =
    match (v1, v2) with
    | S32 i1, S32 i2 -> Int32.equal i1 i2
    | S64 i1, S64 i2 -> Int64.equal i1 i2
    | _ -> false

  let to_string (v : t) : string =
    match v with
    | S32 i -> Format.sprintf "(i32 %ld)" i
    | S64 i -> Format.sprintf "(i64 %Ld)" i
end

module FP = struct
  type ('a, 'b) op =
    | S32 : 'a -> ('a, 'b) op
    | S64 : 'b -> ('a, 'b) op

  type t = (int32, int64) op

  let equal (v1 : t) (v2 : t) : bool =
    match (v1, v2) with
    | S32 i1, S32 i2 -> Int32.equal i1 i2
    | S64 i1, S64 i2 -> Int64.equal i1 i2
    | _ -> false

  let to_string (v : t) : string =
    match v with
    | S32 i -> Format.sprintf "(f32 %ld)" i
    | S64 i -> Format.sprintf "(f64 %Ld)" i
end

type _ t =
  | Int : int -> int t
  | Real : float -> float t
  | Bool : bool -> bool t
  | Bv : BV.t -> BV.t t
  | Fp : FP.t -> FP.t t
  | Str : string -> string t

type value = V : 'a t -> value

let equal (type a) (v1 : a t) (v2 : a t) : bool =
  match (v1, v2) with
  | Int x1, Int x2 -> Int.equal x1 x2
  | Real y1, Real y2 -> Float.equal y1 y2
  | Bool x1, Bool x2 -> Bool.equal x1 x2
  | Str x1, Str x2 -> String.equal x1 x2
  | Bv x1, Bv x2 -> BV.equal x1 x2
  | Fp x1, Fp x2 -> FP.equal x1 x2
  | Bv _, Fp _ | Fp _, Bv _ -> false

module Pp = struct
  let pp (type a) (v : a t) : string =
    match v with
    | Int x -> Int.to_string x
    | Real x -> Float.to_string x
    | Bool x -> Bool.to_string x
    | Str x -> "\"" ^ x ^ "\""
    | Bv x -> BV.to_string x
    | Fp x -> FP.to_string x

  let pp_value (V v : value) : string = pp v
end
