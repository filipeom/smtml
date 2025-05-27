open Alcotest
open Smtml

(* Helpers *)
let value = testable Value.pp Value.equal

let true_ = Value.True

let false_ = Value.False

let int x = Value.Int x

let str x = Value.Str x

let real x = Value.Real x

let int8 x = Value.Bitv (Bitvector.of_int8 x)

let int32 x = Value.Bitv (Bitvector.of_int32 x)

let int64 x = Value.Bitv (Bitvector.of_int64 x)

let float32 x = Value.Num (F32 (Int32.bits_of_float x))

let float64 x = Value.Num (F64 (Int64.bits_of_float x))

let check_type_error f =
  match_raises "raise TypeError"
    (function Eval.TypeError _ -> true | _ -> false)
    f

let check_parse_error f =
  match_raises "raise Invalid_argument"
    (function Invalid_argument _ -> true | _ -> false)
    f

let assert_bool msg real = check bool msg true real

module Int_test = struct
  (* Unary operators *)
  let unop =
    let test_neg _ =
      let result = Eval.unop Ty_int Neg (int 5) in
      check value "" (int (-5)) result
    in
    let test_lognot _ =
      let result = Eval.unop Ty_int Not (int ~-1) in
      check value "" (int 0) result
    in
    let test_abs _ =
      let result = Eval.unop Ty_int Abs (int (-7)) in
      check value "" (int 7) result
    in
    let test_type_error _ =
      check_type_error @@ fun () -> ignore @@ Eval.unop Ty_int Neg (str "hi")
    in
    [ test_case "test_neg" `Quick test_neg
    ; test_case "test_not" `Quick test_lognot
    ; test_case "test_abs" `Quick test_abs
    ; test_case "test_unop_type_error" `Quick test_type_error
    ]

  (* Binary operators *)
  let binop =
    let test_add _ =
      let result = Eval.binop Ty_int Add (int 2) (int 3) in
      check value "" (int 5) result
    in
    let test_sub _ =
      let result = Eval.binop Ty_int Sub (int 3) (int 2) in
      check value "" (int 1) result
    in
    let test_mul _ =
      let result = Eval.binop Ty_int Mul (int 3) (int 3) in
      check value "" (int 9) result
    in
    let test_div _ =
      let result = Eval.binop Ty_int Div (int 6) (int 3) in
      check value "" (int 2) result
    in
    let test_divide_by_zero _ =
      check_raises "1 / 0" Division_by_zero @@ fun () ->
      let _ = Eval.binop Ty_int Div (int 1) (int 0) in
      ()
    in
    let test_rem _ =
      let result = Eval.binop Ty_int Rem (int 6) (int 3) in
      check value "" (int 0) result
    in
    let test_pow _ =
      let result = Eval.binop Ty_int Pow (int 2) (int 3) in
      check value "" (int 8) result
    in
    let test_min_max _ =
      let a = int 42 in
      let b = int 1337 in
      let result = Eval.binop Ty_int Max a b in
      check value "" (int 1337) result;
      let result = Eval.binop Ty_int Min a b in
      check value "" (int 42) result
    in
    let test_logical_ops _ =
      let a = int 0b1100 in
      let b = int 0b1010 in
      check value "" (Eval.binop Ty_int And a b) (int 8);
      check value "" (Eval.binop Ty_int Or a b) (int 14);
      check value "" (Eval.binop Ty_int Xor a b) (int 6)
    in
    let test_shifts _ =
      let i = int 0b0011 in
      check value "" (Eval.binop Ty_int Shl i (int 1)) (int 0b0110);
      check value "" (Eval.binop Ty_int ShrL i (int 1)) (int 0b0001);
      check value "" (Eval.binop Ty_int ShrA i (int 1)) (int 0b0001)
    in
    [ test_case "test_add" `Quick test_add
    ; test_case "test_sub" `Quick test_sub
    ; test_case "test_mul" `Quick test_mul
    ; test_case "test_div" `Quick test_div
    ; test_case "test_divide_by_zero" `Quick test_divide_by_zero
    ; test_case "test_rem" `Quick test_rem
    ; test_case "test_pow" `Quick test_pow
    ; test_case "test_min_max" `Quick test_min_max
    ; test_case "test_logical_ops" `Quick test_logical_ops
    ; test_case "test_shifts" `Quick test_shifts
    ]

  (* Relational operators *)
  let relop =
    let test_lt _ =
      assert_bool "2 < 3" (Eval.relop Ty_int Lt (int 2) (int 3))
    in
    let test_le _ =
      assert_bool "2 <= 3" (Eval.relop Ty_int Le (int 3) (int 3))
    in
    let test_gt _ =
      assert_bool "4 > 4" (Eval.relop Ty_int Gt (int 4) (int 3))
    in
    let test_ge _ =
      assert_bool "4 >= 4" (Eval.relop Ty_int Ge (int 4) (int 4))
    in

    [ test_case "test_lt" `Quick test_lt
    ; test_case "test_le" `Quick test_le
    ; test_case "test_gt" `Quick test_gt
    ; test_case "test_ge" `Quick test_ge
    ]

  (* Conversion operators *)
  let cvtop =
    let test_of_bool _ =
      let result = Eval.cvtop Ty_int OfBool True in
      check value "" (int 1) result
    in
    let test_to_string _ =
      let result = Eval.cvtop Ty_int ToString (int 42) in
      check value "" (str "42") result
    in
    let test_of_string _ =
      let result = Eval.cvtop Ty_int OfString (str "123") in
      check value "" (int 123) result
    in
    let test_of_string_error _ =
      check_parse_error @@ fun () ->
      let _ = Eval.cvtop Ty_int OfString (str "not_an_int") in
      ()
    in
    let test_reinterpret_float _ =
      let result = Eval.cvtop Ty_int Reinterpret_float (real 42.0) in
      check value "" (int 42) result
    in
    [ test_case "test_of_bool" `Quick test_of_bool
    ; test_case "test_to_string" `Quick test_to_string
    ; test_case "test_of_string" `Quick test_of_string
    ; test_case "test_of_string_error" `Quick test_of_string_error
    ; test_case "test_reinterpret_float" `Quick test_reinterpret_float
    ]
end

module Real_test = struct
  (* Unary operators *)
  let unop =
    let test_neg _ =
      let result = Eval.unop Ty_real Neg (real 5.) in
      check value "" (real (-5.)) result
    in
    let test_abs _ =
      let result = Eval.unop Ty_real Abs (real (-7.)) in
      check value "" (real 7.) result
    in
    let test_sqrt _ =
      let result = Eval.unop Ty_real Sqrt (real 9.) in
      check value "" (real 3.) result
    in
    let test_nearest _ =
      check value "" (real 4.) (Eval.unop Ty_real Nearest (real 4.2));
      check value "" (real 5.) (Eval.unop Ty_real Nearest (real 4.6))
    in
    let test_ceil _ =
      let result = Eval.unop Ty_real Ceil (real 4.2) in
      check value "" (real 5.) result
    in
    let test_floor _ =
      let result = Eval.unop Ty_real Floor (real 4.2) in
      check value "" (real 4.) result
    in
    let test_trunc _ =
      let result = Eval.unop Ty_real Trunc (real Float.pi) in
      check value "" (real 3.) result
    in
    let test_is_nan _ =
      check value "" (Eval.unop Ty_real Is_nan (real Float.nan)) true_;
      check value "" (Eval.unop Ty_real Is_nan (real 42.)) false_
    in
    let test_type_error _ =
      check_type_error @@ fun () -> ignore @@ Eval.unop Ty_real Neg (str "hi")
    in
    [ test_case "test_neg" `Quick test_neg
    ; test_case "test_abs" `Quick test_abs
    ; test_case "test_sqrt" `Quick test_sqrt
    ; test_case "test_nearest" `Quick test_nearest
    ; test_case "test_ceil" `Quick test_ceil
    ; test_case "test_floor" `Quick test_floor
    ; test_case "test_trunc" `Quick test_trunc
    ; test_case "test_is_nan" `Quick test_is_nan
    ; test_case "test_unop_type_error" `Quick test_type_error
    ]

  (* Binary operators *)
  let binop =
    let test_add _ =
      let result = Eval.binop Ty_real Add (real 2.) (real 3.) in
      check value "" (real 5.) result
    in
    let test_sub _ =
      let result = Eval.binop Ty_real Sub (real 3.) (real 2.) in
      check value "" (real 1.) result
    in
    let test_mul _ =
      let result = Eval.binop Ty_real Mul (real 3.) (real 3.) in
      check value "" (real 9.) result
    in
    let test_div _ =
      let result = Eval.binop Ty_real Div (real 6.) (real 3.) in
      check value "" (real 2.) result
    in
    let test_divide_by_zero _ =
      let result = Eval.binop Ty_real Div (real 1.) (real 0.) in
      check value "" (real Float.infinity) result
    in
    let test_rem _ =
      let result = Eval.binop Ty_real Rem (real 6.) (real 3.) in
      check value "" (real 0.) result
    in
    let test_pow _ =
      let result = Eval.binop Ty_real Pow (real 2.) (real 3.) in
      check value "" (real 8.) result
    in
    let test_min_max _ =
      let a = real 42. in
      let b = real 1337. in
      let result = Eval.binop Ty_real Max a b in
      check value "" (real 1337.) result;
      let result = Eval.binop Ty_real Min a b in
      check value "" (real 42.) result
    in
    [ test_case "test_add" `Quick test_add
    ; test_case "test_sub" `Quick test_sub
    ; test_case "test_mul" `Quick test_mul
    ; test_case "test_div" `Quick test_div
    ; test_case "test_divide_by_zero" `Quick test_divide_by_zero
    ; test_case "test_rem" `Quick test_rem
    ; test_case "test_pow" `Quick test_pow
    ; test_case "test_min_max" `Quick test_min_max
    ]

  (* Relational operators *)
  let relop =
    let test_eq _ =
      assert_bool "0 = 0" (Eval.relop Ty_real Eq (real 0.0) (real 0.0));
      assert_bool "nan != nan"
        (not (Eval.relop Ty_real Eq (real Float.nan) (real Float.nan)))
    in
    let test_ne _ =
      assert_bool "0 = 0" (not (Eval.relop Ty_real Ne (real 0.0) (real 0.0)));
      assert_bool "nan != nan"
        (Eval.relop Ty_real Ne (real Float.nan) (real Float.nan))
    in
    let test_lt _ =
      assert_bool "2 < 3" (Eval.relop Ty_real Lt (real 2.) (real 3.))
    in
    let test_le _ =
      assert_bool "2 <= 3" (Eval.relop Ty_real Le (real 3.) (real 3.))
    in
    let test_gt _ =
      assert_bool "4 > 4" (Eval.relop Ty_real Gt (real 4.) (real 3.))
    in
    let test_ge _ =
      assert_bool "4 >= 4" (Eval.relop Ty_real Ge (real 4.) (real 4.))
    in

    [ test_case "test_eq" `Quick test_eq
    ; test_case "test_ne" `Quick test_ne
    ; test_case "test_lt" `Quick test_lt
    ; test_case "test_le" `Quick test_le
    ; test_case "test_gt" `Quick test_gt
    ; test_case "test_ge" `Quick test_ge
    ]

  (* Conversion operators *)
  let cvtop =
    let test_of_string _ =
      let result = Eval.cvtop Ty_real OfString (str "42.") in
      check value "" (real 42.) result
    in
    let test_to_string _ =
      let result = Eval.cvtop Ty_real ToString (real 42.) in
      check value "" (str "42.") result
    in
    let test_of_string_error _ =
      check_parse_error @@ fun () ->
      let _ = Eval.cvtop Ty_real OfString (str "not_a_real") in
      ()
    in
    let test_reinterpret_int _ =
      let result = Eval.cvtop Ty_real Reinterpret_int (int 42) in
      check value "" (real 42.) result
    in
    let test_reinterpret_float _ =
      let result = Eval.cvtop Ty_real Reinterpret_float (real 42.) in
      check value "" (int 42) result
    in
    [ test_case "test_to_string" `Quick test_to_string
    ; test_case "test_of_string" `Quick test_of_string
    ; test_case "test_of_string_error" `Quick test_of_string_error
    ; test_case "test_reinterpret_int" `Quick test_reinterpret_int
    ; test_case "test_reinterpret_float" `Quick test_reinterpret_float
    ]
end

module Bool_test = struct
  (* Unary operators *)
  let unop =
    [ ( test_case "test_not" `Quick @@ fun _ ->
        let result = Eval.unop Ty_bool Not true_ in
        check value "" false_ result )
    ; ( test_case "test_type_error" `Quick @@ fun _ ->
        check_type_error @@ fun () ->
        ignore @@ Eval.unop Ty_bool Not (str "false") )
    ]

  (* Binary operators *)
  let binop =
    [ ( test_case "test_and" `Quick @@ fun _ ->
        check value "" true_ (Eval.binop Ty_bool And true_ true_);
        check value "" false_ (Eval.binop Ty_bool And true_ false_);
        check value "" false_ (Eval.binop Ty_bool And false_ true_);
        check value "" false_ (Eval.binop Ty_bool And false_ false_) )
    ; ( test_case "test_or" `Quick @@ fun _ ->
        check value "" true_ (Eval.binop Ty_bool Or true_ true_);
        check value "" true_ (Eval.binop Ty_bool Or true_ false_);
        check value "" true_ (Eval.binop Ty_bool Or false_ true_);
        check value "" false_ (Eval.binop Ty_bool Or false_ false_) )
    ; ( test_case "test_xor" `Quick @@ fun _ ->
        check value "" false_ (Eval.binop Ty_bool Xor true_ true_);
        check value "" true_ (Eval.binop Ty_bool Xor true_ false_);
        check value "" true_ (Eval.binop Ty_bool Xor false_ true_);
        check value "" false_ (Eval.binop Ty_bool Xor false_ false_) )
    ]

  let triop =
    [ ( test_case "test_ite" `Quick @@ fun _ ->
        let result = Eval.triop Ty_bool Ite true_ (int 1) (int 0) in
        check value "" (int 1) result;
        let result = Eval.triop Ty_bool Ite false_ (int 1) (int 0) in
        check value "" (int 0) result )
    ]

  (* Relational operators *)
  let relop =
    [ ( test_case "test_eq" `Quick @@ fun _ ->
        assert_bool "0 = 0" (Eval.relop Ty_bool Eq (int 0) (int 0));
        assert_bool "\"abc\" = \"abc\""
          (Eval.relop Ty_bool Eq (str "abc") (str "abc"));
        assert_bool "True = True" (Eval.relop Ty_bool Eq true_ true_);
        assert_bool "0l = 0l" (Eval.relop (Ty_bitv 32) Eq (int32 0l) (int32 0l))
      )
    ; ( test_case "test_ne" `Quick @@ fun _ ->
        assert_bool "0 != 1" (Eval.relop Ty_bool Ne (int 0) (int 1));
        assert_bool "\"abc\" != \"cba\""
          (Eval.relop Ty_bool Ne (str "abc") (str "cba"));
        assert_bool "True != False" (Eval.relop Ty_bool Ne true_ false_);
        assert_bool "0l != 1l" (Eval.relop Ty_bool Ne (int32 0l) (int32 1l)) )
    ]

  let naryop =
    [ ( test_case "test_logical_ops" `Quick @@ fun _ ->
        let l = [ true_; false_; true_; false_ ] in
        check value "" false_ (Eval.naryop Ty_bool Logand l);
        check value "" true_ (Eval.naryop Ty_bool Logor l) )
    ]
end

module Str_test = struct
  (* Unary operators *)
  let unop =
    [ ( test_case "test_length" `Quick @@ fun _ ->
        let result = Eval.unop Ty_str Length (str "abc") in
        check value "" (int 3) result )
    ; ( test_case "test_trim" `Quick @@ fun _ ->
        let result = Eval.unop Ty_str Trim (str "abc\n") in
        check value "" (str "abc") result )
    ; ( test_case "test_type_error" `Quick @@ fun _ ->
        check_type_error @@ fun () -> ignore @@ Eval.unop Ty_str Length (int 42)
      )
    ]

  (* Binary operators *)
  let binop =
    [ ( test_case "test_at" `Quick @@ fun _ ->
        let result = Eval.binop Ty_str At (str "abc") (int 0) in
        check value "" (str "a") result )
    ; ( test_case "test_index_out_of_bounds_error" `Quick @@ fun _ ->
        check_raises "\"abc\"[4]" Eval.Index_out_of_bounds @@ fun () ->
        let result = Eval.binop Ty_str At (str "abc") (int 4) in
        check value "" (str "a") result )
    ; ( test_case "test_string_prefix" `Quick @@ fun _ ->
        let result = Eval.binop Ty_str String_prefix (str "ab") (str "abc") in
        check value "" true_ result )
    ; ( test_case "test_string_suffix" `Quick @@ fun _ ->
        let result = Eval.binop Ty_str String_suffix (str "bc") (str "abc") in
        check value "" true_ result )
    ; ( test_case "test_string_suffix" `Quick @@ fun _ ->
        let result =
          Eval.binop Ty_str String_contains (str "abcd") (str "bc")
        in
        check value "" true_ result )
    ]

  let triop =
    [ ( test_case "test_string_extract" `Quick @@ fun _ ->
        let result =
          Eval.triop Ty_str String_extract (str "aadd") (int 1) (int 2)
        in
        check value "" (str "ad") result )
    ; ( test_case "test_string_replace" `Quick @@ fun _ ->
        let result =
          Eval.triop Ty_str String_replace (str "aadd") (str "ad") (str "bc")
        in
        check value "" (str "abcd") result )
    ; ( test_case "test_string_index" `Quick @@ fun _ ->
        let result =
          Eval.triop Ty_str String_index (str "abcd") (str "bc") (int 0)
        in
        check value "" (int 1) result )
    ]

  (* Relational operators *)
  let relop =
    [ ( test_case "test_lt" `Quick @@ fun _ ->
        assert_bool "a < b" (Eval.relop Ty_str Lt (str "a") (str "b")) )
    ; ( test_case "test_le" `Quick @@ fun _ ->
        assert_bool "a <= a" (Eval.relop Ty_str Le (str "a") (str "a")) )
    ; ( test_case "test_gt" `Quick @@ fun _ ->
        assert_bool "b > a" (Eval.relop Ty_str Gt (str "b") (str "a")) )
    ; ( test_case "test_ge" `Quick @@ fun _ ->
        assert_bool "a >= a" (Eval.relop Ty_str Ge (str "a") (str "a")) )
    ; ( test_case "test_eq" `Quick @@ fun _ ->
        assert_bool "hello = hello"
          (Eval.relop Ty_str Eq (str "hello") (str "hello")) )
    ; ( test_case "test_ne" `Quick @@ fun _ ->
        assert_bool "foo != bar" (Eval.relop Ty_str Ne (str "foo") (str "bar"))
      )
    ]

  let cvtop =
    [ ( test_case "test_string_to_code" `Quick @@ fun _ ->
        let result = Eval.cvtop Ty_str String_to_code (str "a") in
        check value "" (int 97) result )
    ; ( test_case "test_string_from_code" `Quick @@ fun _ ->
        let result = Eval.cvtop Ty_str String_from_code (int 98) in
        check value "" (str "b") result )
    ; ( test_case "test_string_to_int" `Quick @@ fun _ ->
        let result = Eval.cvtop Ty_str String_to_int (str "98") in
        check value "" (int 98) result )
    ; ( test_case "test_string_to_int_raises" `Quick @@ fun _ ->
        check_parse_error @@ fun () ->
        let _ = Eval.cvtop Ty_str String_to_int (str "not_an_int") in
        () )
    ; ( test_case "test_string_from_int" `Quick @@ fun _ ->
        let result = Eval.cvtop Ty_str String_from_int (int 97) in
        check value "" (str "97") result )
    ; ( test_case "test_string_to_float" `Quick @@ fun _ ->
        let result = Eval.cvtop Ty_str String_to_float (str "98") in
        check value "" (real 98.) result )
    ; ( test_case "test_string_to_float_raises" `Quick @@ fun _ ->
        check_parse_error @@ fun () ->
        let _ = Eval.cvtop Ty_str String_to_float (str "not_a_real") in
        () )
    ]

  let naryop =
    [ ( test_case "test_logical_ops" `Quick @@ fun _ ->
        let l = [ str "a"; str "b"; str "c"; str "d" ] in
        check value "" (str "abcd") (Eval.naryop Ty_str Concat l) )
    ]
end

module Float_test (FXX : sig
  val ty : Ty.t

  val v : float -> Value.t
end) =
struct
  open FXX

  (* Unary operators *)
  let unop =
    let test_neg _ =
      let result = Eval.unop ty Neg (v 5.) in
      check value "" (v (-5.)) result
    in
    let test_abs _ =
      let result = Eval.unop ty Abs (v (-7.)) in
      check value "" (v 7.) result
    in
    let test_sqrt _ =
      let result = Eval.unop ty Sqrt (v 9.) in
      check value "" (v 3.) result
    in
    let test_nearest _ =
      check value "" (v 4.) (Eval.unop ty Nearest (v 4.2));
      check value "" (v 5.) (Eval.unop ty Nearest (v 4.6))
    in
    let test_ceil _ =
      let result = Eval.unop ty Ceil (v 4.2) in
      check value "" (v 5.) result
    in
    let test_floor _ =
      let result = Eval.unop ty Floor (v 4.2) in
      check value "" (v 4.) result
    in
    let test_trunc _ =
      let result = Eval.unop ty Trunc (v Float.pi) in
      check value "" (v 3.) result
    in
    let test_is_nan _ =
      check value "" (Eval.unop ty Is_nan (v Float.nan)) true_;
      check value "" (Eval.unop ty Is_nan (v 42.)) false_
    in
    let test_type_error _ =
      check_type_error @@ fun () -> ignore @@ Eval.unop ty Neg (str "hi")
    in
    [ test_case "test_neg" `Quick test_neg
    ; test_case "test_abs" `Quick test_abs
    ; test_case "test_sqrt" `Quick test_sqrt
    ; test_case "test_nearest" `Quick test_nearest
    ; test_case "test_ceil" `Quick test_ceil
    ; test_case "test_floor" `Quick test_floor
    ; test_case "test_trunc" `Quick test_trunc
    ; test_case "test_is_nan" `Quick test_is_nan
    ; test_case "test_unop_type_error" `Quick test_type_error
    ]

  (* Binary operators *)
  let binop =
    let test_add _ =
      let result = Eval.binop ty Add (v 2.) (v 3.) in
      check value "" (v 5.) result
    in
    let test_sub _ =
      let result = Eval.binop ty Sub (v 3.) (v 2.) in
      check value "" (v 1.) result
    in
    let test_mul _ =
      let result = Eval.binop ty Mul (v 3.) (v 3.) in
      check value "" (v 9.) result
    in
    let test_div _ =
      let result = Eval.binop ty Div (v 6.) (v 3.) in
      check value "" (v 2.) result
    in
    let test_divide_by_zero _ =
      let result = Eval.binop ty Div (v 1.) (v 0.) in
      check value "" (v Float.infinity) result
    in
    let test_rem _ =
      let result = Eval.binop ty Rem (v 6.) (v 3.) in
      check value "" (v 0.) result
    in
    let test_min_max _ =
      let a = v 42. in
      let b = v 1337. in
      let result = Eval.binop ty Max a b in
      check value "" (v 1337.) result;
      let result = Eval.binop ty Min a b in
      check value "" (v 42.) result
    in
    let test_copysign _ =
      let result = Eval.binop ty Copysign (v (-2.)) (v 3.) in
      check value "" (v 2.) result
    in
    [ test_case "test_add" `Quick test_add
    ; test_case "test_sub" `Quick test_sub
    ; test_case "test_mul" `Quick test_mul
    ; test_case "test_div" `Quick test_div
    ; test_case "test_divide_by_zero" `Quick test_divide_by_zero
    ; test_case "test_rem" `Quick test_rem
    ; test_case "test_min_max" `Quick test_min_max
    ; test_case "test_copysign" `Quick test_copysign
    ]

  (* Relational operators *)
  let relop =
    let test_eq _ =
      assert_bool "0 = 0" (Eval.relop ty Eq (v 0.0) (v 0.0));
      assert_bool "nan != nan"
        (not (Eval.relop ty Eq (v Float.nan) (v Float.nan)))
    in
    let test_ne _ =
      assert_bool "0 = 0" (not (Eval.relop ty Ne (v 0.0) (v 0.0)));
      assert_bool "nan != nan" (Eval.relop ty Ne (v Float.nan) (v Float.nan))
    in
    let test_lt _ = assert_bool "2 < 3" (Eval.relop ty Lt (v 2.) (v 3.)) in
    let test_le _ = assert_bool "2 <= 3" (Eval.relop ty Le (v 3.) (v 3.)) in
    let test_gt _ = assert_bool "4 > 4" (Eval.relop ty Gt (v 4.) (v 3.)) in
    let test_ge _ = assert_bool "4 >= 4" (Eval.relop ty Ge (v 4.) (v 4.)) in

    [ test_case "test_eq" `Quick test_eq
    ; test_case "test_ne" `Quick test_ne
    ; test_case "test_lt" `Quick test_lt
    ; test_case "test_le" `Quick test_le
    ; test_case "test_gt" `Quick test_gt
    ; test_case "test_ge" `Quick test_ge
    ]
end

module F32_test = struct
  include Float_test (struct
    let ty = Ty.Ty_fp 32

    let v = float32
  end)

  let regression =
    [ ( test_case "test_neg_non_canonical_nan" `Quick @@ fun _ ->
        let nan = Value.Num (F32 0xff8a1d2bl) in
        let i32 =
          Eval.cvtop (Ty_bitv 32) Reinterpret_float
          @@ Eval.unop (Ty_fp 32) Neg nan
        in
        let expected = Value.Bitv (Bitvector.of_int32 2139757867l) in
        check value "" i32 expected )
    ]
end

module F64_test = Float_test (struct
  let ty = Ty.Ty_fp 64

  let v = float64
end)

module I32Cvtop_test = struct
  let ty = Ty.Ty_bitv 32

  let cvtop =
    [ ( test_case "test_wrap_i64" `Quick @@ fun _ ->
        check value "" (int32 0l) (Eval.cvtop ty WrapI64 (int64 0x1_0000_0000L))
      )
    ; ( test_case "test_truncsf32" `Quick @@ fun _ ->
        check value "" (int32 3l) (Eval.cvtop ty TruncSF32 (float32 3.7)) )
    ; ( test_case "test_truncuf32" `Quick @@ fun _ ->
        check value "" (int32 4l) (Eval.cvtop ty TruncUF32 (float32 4.9)) )
    ; ( test_case "test_truncsf64" `Quick @@ fun _ ->
        check value "" (int32 (-5l)) (Eval.cvtop ty TruncSF64 (float64 (-5.2)))
      )
    ; ( test_case "test_truncuf64" `Quick @@ fun _ ->
        check value "" (int32 6l) (Eval.cvtop ty TruncUF64 (float64 6.99)) )
    ; ( test_case "test_trunc_sat_f32_s" `Quick @@ fun _ ->
        check value "" (int32 7l) (Eval.cvtop ty Trunc_sat_f32_s (float32 7.5))
      )
    ; ( test_case "test_trunc_sat_f32_u" `Quick @@ fun _ ->
        check value "" (int32 8l) (Eval.cvtop ty Trunc_sat_f32_u (float32 8.5))
      )
    ; ( test_case "test_trunc_sat_f64_s" `Quick @@ fun _ ->
        check value "" (int32 (-9l))
          (Eval.cvtop ty Trunc_sat_f64_s (float64 (-9.9))) )
    ; ( test_case "test_trunc_sat_f64_u" `Quick @@ fun _ ->
        check value "" (int32 10l)
          (Eval.cvtop ty Trunc_sat_f64_u (float64 10.9)) )
    ; ( test_case "test_reinterpret_float" `Quick @@ fun _ ->
        check value ""
          (int32 (Int32.bits_of_float 1.5))
          (Eval.cvtop ty Reinterpret_float (float32 1.5)) )
    ; ( test_case "test_sign_extend" `Quick @@ fun _ ->
        check value "" (int32 (-1l))
          (Eval.cvtop ty (Sign_extend 24) (int8 0xff)) )
    ; ( test_case "test_zero_extend" `Quick @@ fun _ ->
        check value "" (int32 0xffl)
          (Eval.cvtop ty (Zero_extend 24) (int8 0xff)) )
    ]
end

module I64Cvtop_test = struct
  let ty = Ty.Ty_bitv 64

  let cvtop =
    [ ( test_case "test_wrap_i64_error" `Quick @@ fun _ ->
        check_type_error @@ fun () ->
        let _ = Eval.cvtop ty WrapI64 (int64 0x1_0000_0000L) in
        () )
    ; ( test_case "test_truncsf32" `Quick @@ fun _ ->
        check value "" (int64 3L) (Eval.cvtop ty TruncSF32 (float32 3.7)) )
    ; ( test_case "test_truncuf32" `Quick @@ fun _ ->
        check value "" (int64 4L) (Eval.cvtop ty TruncUF32 (float32 4.9)) )
    ; ( test_case "test_truncsf64" `Quick @@ fun _ ->
        check value "" (int64 (-5L)) (Eval.cvtop ty TruncSF64 (float64 (-5.2)))
      )
    ; ( test_case "test_truncuf64" `Quick @@ fun _ ->
        check value "" (int64 6L) (Eval.cvtop ty TruncUF64 (float64 6.99)) )
    ; ( test_case "test_trunc_sat_f32_s" `Quick @@ fun _ ->
        check value "" (int64 7L) (Eval.cvtop ty Trunc_sat_f32_s (float32 7.5))
      )
    ; ( test_case "test_trunc_sat_f32_u" `Quick @@ fun _ ->
        check value "" (int64 8L) (Eval.cvtop ty Trunc_sat_f32_u (float32 8.5))
      )
    ; ( test_case "test_trunc_sat_f64_s" `Quick @@ fun _ ->
        check value "" (int64 (-9L))
          (Eval.cvtop ty Trunc_sat_f64_s (float64 (-9.9))) )
    ; ( test_case "test_trunc_sat_f64_u" `Quick @@ fun _ ->
        check value "" (int64 10L)
          (Eval.cvtop ty Trunc_sat_f64_u (float64 10.9)) )
    ; ( test_case "test_reinterpret_float" `Quick @@ fun _ ->
        check value ""
          (int64 (Int64.bits_of_float 1.5))
          (Eval.cvtop ty Reinterpret_float (float64 1.5)) )
    ; ( test_case "test_sign_extend_i8" `Quick @@ fun _ ->
        check value "" (int64 (-1L))
          (Eval.cvtop ty (Sign_extend 56) (int8 (-1))) )
    ; ( test_case "test_sign_extend_i32" `Quick @@ fun _ ->
        check value "" (int64 (-1L))
          (Eval.cvtop ty (Sign_extend 32) (int32 (-1l))) )
    ; ( test_case "test_zero_extend" `Quick @@ fun _ ->
        check value "" (int64 0xffffffffL)
          (Eval.cvtop ty (Zero_extend 32) (int32 (-1l))) )
    ]
end

module F32Cvtop_test = struct
  let ty = Ty.Ty_fp 32

  let cvtop =
    [ ( test_case "test_demote_f64" `Quick @@ fun _ ->
        let result = Eval.cvtop ty DemoteF64 (float64 3.14) in
        check value "" (float32 3.14) result )
    ; ( test_case "test_demote_f64_nan" `Quick @@ fun _ ->
        let result = Eval.cvtop ty DemoteF64 (float64 nan) in
        check value "" (float32 nan) result )
    ; ( test_case "test_convert_si32" `Quick @@ fun _ ->
        let result = Eval.cvtop ty ConvertSI32 (int32 (-42l)) in
        check value "" (float32 (-42.)) result )
    ; ( test_case "test_convert_ui32" `Quick @@ fun _ ->
        let result = Eval.cvtop ty ConvertUI32 (int32 42l) in
        check value "" (float32 42.) result;
        let result = Eval.cvtop ty ConvertUI32 (int32 (-1l)) in
        check value "" (float32 4294967294.) result )
    ; ( test_case "test_convert_si64" `Quick @@ fun _ ->
        let result = Eval.cvtop ty ConvertSI64 (int64 (-42L)) in
        check value "" (float32 (-42.)) result;
        let result = Eval.cvtop ty ConvertSI64 (int64 0x10_0000_0000_0100L) in
        check value "" (float32 4503599627370512.) result )
    ; ( test_case "test_convert_ui64" `Quick @@ fun _ ->
        let result = Eval.cvtop ty ConvertUI64 (int64 42L) in
        check value "" (float32 42.) result;
        let result = Eval.cvtop ty ConvertUI64 (int64 0x10_0000_0000_0100L) in
        check value "" (float32 4503599627370512.) result )
    ; ( test_case "test_reinterpret_int" `Quick @@ fun _ ->
        let result = Eval.cvtop ty Reinterpret_int (int32 1065353216l) in
        check value "" (float32 1.) result )
    ; ( test_case "test_promote_f32_error" `Quick @@ fun _ ->
        check_type_error @@ fun () ->
        let _ = Eval.cvtop ty PromoteF32 (float32 42.0) in
        () )
    ]
end

module F64Cvtop_test = struct
  let ty = Ty.Ty_fp 64

  let cvtop =
    [ ( test_case "test_promote_f32" `Quick @@ fun _ ->
        let result = Eval.cvtop ty PromoteF32 (float32 42.0) in
        check value "" (float64 42.0) result )
    ; ( test_case "test_promote_f32_nan" `Quick @@ fun _ ->
        let result = Eval.cvtop ty PromoteF32 (float32 nan) in
        check value "" (float64 nan) result )
    ; ( test_case "test_convert_si32" `Quick @@ fun _ ->
        let result = Eval.cvtop ty ConvertSI32 (int32 (-42l)) in
        check value "" (float64 (-42.)) result )
    ; ( test_case "test_convert_ui32" `Quick @@ fun _ ->
        let result = Eval.cvtop ty ConvertUI32 (int32 42l) in
        check value "" (float64 42.) result )
    ; ( test_case "test_convert_si64" `Quick @@ fun _ ->
        let result = Eval.cvtop ty ConvertSI64 (int64 (-42L)) in
        check value "" (float64 (-42.)) result )
    ; ( test_case "test_convert_ui64" `Quick @@ fun _ ->
        let result = Eval.cvtop ty ConvertUI64 (int64 42L) in
        check value "" (float64 42.) result;
        let result = Eval.cvtop ty ConvertUI64 (int64 (-1L)) in
        check value "" (float64 (9223372036854775807. *. 2.)) result )
    ; ( test_case "test_reinterpret_int" `Quick @@ fun _ ->
        let result =
          Eval.cvtop ty Reinterpret_int (int64 4607182418800017408L)
        in
        check value "" (float64 1.) result )
    ; ( test_case "test_demote_f64_error" `Quick @@ fun _ ->
        check_type_error @@ fun () ->
        let _ = Eval.cvtop ty DemoteF64 (float64 3.14) in
        () )
    ]
end

let testsuite : unit test list =
  [ ("Eval.Int.unop", Int_test.unop)
  ; ("Eval.Real.unop", Real_test.unop)
  ; ("Eval.Bool.unop", Bool_test.unop)
  ; ("Eval.Str.unop", Str_test.unop)
  ; ("Eval.F32.unop", F32_test.unop)
  ; ("Eval.F64.unop", F64_test.unop)
  ; ("Eval.Int.binop", Int_test.binop)
  ; ("Eval.Real.binop", Real_test.binop)
  ; ("Eval.Bool.binop", Bool_test.binop)
  ; ("Eval.Str.binop", Str_test.binop)
  ; ("Eval.F32.binop", F32_test.binop)
  ; ("Eval.F64.binop", F64_test.binop)
  ; ("Eval.Bool.triop", Bool_test.triop)
  ; ("Eval.Str.triop", Str_test.triop)
  ; ("Eval.Int.relop", Int_test.relop)
  ; ("Eval.Real.relop", Real_test.relop)
  ; ("Eval.Bool.relop", Bool_test.relop)
  ; ("Eval.Str.relop", Str_test.relop)
  ; ("Eval.F32.relop", F32_test.relop)
  ; ("Eval.F64.relop", F64_test.relop)
  ; ("Eval.Int.cvtop", Int_test.cvtop)
  ; ("Eval.Real.cvtop", Real_test.cvtop)
  ; ("Eval.Str.cvtop", Str_test.cvtop)
  ; ("Eval.I32.cvtop", I32Cvtop_test.cvtop)
  ; ("Eval.I64.cvtop", I64Cvtop_test.cvtop)
  ; ("Eval.F32.cvtop", F32Cvtop_test.cvtop)
  ; ("Eval.F64.cvtop", F64Cvtop_test.cvtop)
  ; ("Eval.Bool.naryop", Bool_test.naryop)
  ; ("Eval.Str.naryop", Str_test.naryop)
  ; ("Eval.F32.regression", F32_test.regression)
  ]
