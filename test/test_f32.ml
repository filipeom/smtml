open Encoding
module Batch = Batch.Make (Z3_mappings)

module F32 = FloatingPoint.F32

let solver = Batch.create ()
let x = Expression.mk_symbol_s `F32Type "x"
let nan = F32.const Float.nan

let%test "deterministic_nan" =
  let pc =
    [
      Boolean.mk_not (F32.mk_is_nan x);
      F32.mk_is_nan x
    ]
  in
  false = Batch.check solver pc

let%test "nondeterministic_nan" =
  let pc =
    [ F32.mk_ne x nan; F32.mk_is_nan x]
  in
  true = Batch.check solver pc
