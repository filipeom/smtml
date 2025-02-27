open Smtml

module Make (M : Mappings_intf.S) = struct
  open Test_harness
  module Cached = Solver.Cached (M)
  module Solver = Solver.Incremental (M)

  let () =
    let solver = Cached.create ~logic:LIA () in
    let x = Infix.symbol "x" Ty_int in
    let c = Infix.(Int.(x >= int 0)) in
    assert (Cached.cache_hits () = 0);
    assert_sat (Cached.check_set solver @@ Expr.Set.singleton c);
    assert_sat (Cached.check_set solver @@ Expr.Set.singleton c);
    assert_sat (Cached.check_set solver @@ Expr.Set.singleton c);
    assert (Cached.cache_misses () = 1);
    assert (Cached.cache_hits () = 2)

  let () =
    let open Infix in
    let solver = Solver.create ~logic:LIA () in
    let symbol_x = Symbol.("x" @: Ty_int) in
    let x = Expr.symbol symbol_x in
    assert_sat (Solver.check solver []);

    Solver.push solver;
    Solver.add solver Int.[ x >= int 0 ];
    assert_sat (Solver.check solver []);
    assert (
      let v = Solver.get_value solver x in
      Expr.equal v (int 0) );
    Solver.pop solver 1;

    Solver.push solver;
    Solver.add solver [ x = int 3 ];
    assert_sat (Solver.check solver []);
    assert (
      let v = Solver.get_value solver Int.(x * x) in
      Expr.equal v (int 9) );
    Solver.pop solver 1;

    Solver.push solver;
    Solver.add solver Int.[ x >= int 0 || x < int 0 ];
    assert_sat (Solver.check solver []);
    (* necessary, otherwise the solver doesn't know x and can't produce a model
       for it *)
    let model = Solver.model ~symbols:[ symbol_x ] solver in
    let val_x = Option.bind model (fun m -> Model.evaluate m symbol_x) in
    assert (Option.is_some val_x);
    Solver.pop solver 1;

    Solver.add solver [ x = int 5 ];
    assert_sat (Solver.check solver []);
    let model = Solver.model solver in
    let val_x = Option.bind model (fun m -> Model.evaluate m symbol_x) in
    assert (Stdlib.(Some (Value.Int 5) = val_x))
end
