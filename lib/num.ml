open Core

type t =
  | I32 of int32
  | I64 of int64
  | F32 of int32
  | F64 of int64

let ( = ) (n1 : t) (n2 : t) : bool =
  match (n1, n2) with
  | I32 i1, I32 i2 -> Int32.(i1 = i2)
  | I64 i1, I64 i2 -> Int64.(i1 = i2)
  | F32 i1, F32 i2 -> Int32.(i1 = i2)
  | F64 i1, F64 i2 -> Int64.(i1 = i2)
  | _ -> false

let to_string (n : t) : string =
  match n with
  | I32 i -> sprintf "(i32 %ld)" i
  | I64 i -> sprintf "(i64 %Ld)" i
  | F32 f -> sprintf "(f32 %f)" (Int32.float_of_bits f)
  | F64 f -> sprintf "(f64 %f)" (Int64.float_of_bits f)

let num_of_bool (b : bool) : t = I32 (if b then 1l else 0l)
