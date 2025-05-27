open Alcotest
open Smtml

let test_of_json _ =
  let open Result in
  let model_str =
    {|
      {
        "model" : {
          "x_0" : { "ty" : "int", "value" : 42 },
          "x_1" : { "ty" : "bool", "value" : true },
          "x_2" : { "ty" : "f32", "value" : 42.42 },
          "x_3" : { "ty" : "i64", "value" : -13333337 }
        }
      }
    |}
  in
  let model = Model.Parse.Json.from_string model_str in
  check bool "model is valid" true (match model with Ok _ -> true | _ -> false)

let test_rt_json _ =
  let x = Symbol.make (Ty_bitv 32) "x" in
  let y = Symbol.make (Ty_bitv 64) "y" in
  let z = Symbol.make (Ty_fp 32) "y" in
  let orig_model : Model.t =
    let tbl = Hashtbl.create 16 in
    List.iter
      (fun ((s, v) : Symbol.t * Value.t) -> Hashtbl.replace tbl s v)
      [ (x, Bitv (Bitvector.of_int32 Int32.min_int))
      ; (y, Bitv (Bitvector.of_int64 Int64.max_int))
      ; (z, Num (F32 1084227584l))
      ];
    tbl
  in
  let json_model = Model.to_json_string orig_model in
  match Model.Parse.Json.from_string json_model with
  | Error (`Msg err) -> Fmt.failwith "%s" err
  | Ok model -> begin
    let x_val = Model.evaluate model x in
    let y_val = Model.evaluate model y in
    let z_val = Model.evaluate model z in
    match (x_val, y_val, z_val) with
    | Some x_val, Some y_val, Some z_val ->
      check bool "parsed values are correct" true
        ( Value.equal x_val (Bitv (Bitvector.of_int32 Int32.min_int))
        && Value.equal y_val (Bitv (Bitvector.of_int64 Int64.max_int))
        && Value.equal z_val (Num (F32 (Int32.bits_of_float 5.0))) )
    | _ -> assert false
  end

let test_of_scfg _ =
  let open Result in
  let model_str =
    {|
      model {
        symbol x_0 int 42
        symbol x_1 bool true
        symbol x_2 f32 42.42
        symbol x_3 i64 -13333337
      }
    |}
  in
  let model = Model.Parse.Scfg.from_string model_str in
  check bool "parse valid scfg model" true
    (match model with Ok _ -> true | _ -> false)

let testsuite : unit test list =
  [ ( "Model"
    , [ test_case "test_of_json" `Quick test_of_json
      ; test_case "test_round_trip_json" `Quick test_rt_json
      ; test_case "test_of_scfg" `Quick test_of_scfg
      ] )
  ]
