open Smtml

(* Test Model.to_json *)
let () =
  let x = Symbol.make Ty_int "x" in
  let y = Symbol.make Ty_real "y" in
  let z = Symbol.make Ty_bool "z" in
  let u = Symbol.make Ty_str "u" in
  let model : Model.t =
    let tbl = Hashtbl.create 16 in
    List.iter
      (fun ((s, v) : Symbol.t * Value.t) -> Hashtbl.replace tbl s v)
      [ (x, Int 1); (y, Real 2.0); (z, True); (u, Str "abc") ];
    tbl
  in
  let model_to_json = Model.to_json model in
  Fmt.pr "%a@." (Yojson.pretty_print ~std:true) model_to_json;
  let model_to_scfg = Model.to_scfg ~no_value:false model in
  Fmt.pr "%a@." Scfg.Pp.config model_to_scfg;
  let model_to_sexp = Model.to_sexp model in
  Fmt.pr "%a@." (Sexplib.Sexp.pp_hum_indent 2) model_to_sexp

(* Parsing *)

(* json *)
let () =
  let open Result in
  let model_str =
    {|
      {
        "model" : {
          "x_0" : { "ty" : "int", "value" : 42 },
          "x_1" : { "ty" : "bool", "value" : true },
          "x_2" : { "ty" : "f32", "value" : 42.42 }
        }
      }
    |}
  in
  let model = Model.Parse.Json.from_string model_str in
  assert (match model with Ok _ -> true | _ -> false)

(* scfg *)
let () =
  let open Result in
  let model_str =
    {|
      model {
        symbol x_0 int 42
        symbol x_1 bool true
        symbol x_2 f32 42.42
      }
    |}
  in
  let model = Model.Parse.Scfg.from_string model_str in
  assert (match model with Ok _ -> true | _ -> false)
