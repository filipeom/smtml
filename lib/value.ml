type _ t =
  | Int : int -> int t
  | Real : float -> float t
  | Bool : bool -> bool t
  | Num : Num.t -> Num.t t
  | Str : string -> string t

type value = V : 'a t -> value

let equal (type a) (v1 : a t) (v2 : a t) : bool =
  match (v1, v2) with
  | Int x1, Int x2 -> Int.equal x1 x2
  | Real y1, Real y2 -> Float.equal y1 y2
  | Bool x1, Bool x2 -> Bool.equal x1 x2
  | Num x1, Num x2 -> Num.(x1 = x2)
  | Str x1, Str x2 -> String.equal x1 x2

module Pp = struct
  let pp (type a) (v : a t) : string =
    match v with
    | Int x -> Int.to_string x
    | Real x -> Float.to_string x
    | Bool x -> Bool.to_string x
    | Num x -> Num.to_string x
    | Str x -> "\"" ^ x ^ "\""

  let pp_value (V v : value) : string = pp v
end