open Smtml

module Make (M : Mappings_intf.S) = struct
  open Test_harness
  module Cached = Solver.Cached (M)
  module Solver = Solver.Incremental (M)

  let () =
    let open Infix in
    let solver = Cached.create ~logic:LIA () in
    let x = symbol "x" Ty_int in
    let c = Int.(x >= int 0) in
    assert (Stdlib.Int.equal !Cached.solver_count 0);
    assert_sat (Cached.check_sat solver ~assumptions:[ c ]);
    assert_sat (Cached.check_sat solver ~assumptions:[ c ]);
    assert_sat (Cached.check_sat solver ~assumptions:[ c ]);
    assert (Stdlib.Int.equal !Cached.solver_count 1)

  let () =
    let open Infix in
    let solver = Solver.create ~logic:LIA () in
    let symbol_x = Symbol.("x" @: Ty_int) in
    let x = Expr.mk_symbol symbol_x in
    assert_sat (Solver.check_sat solver ~assumptions:[]);

    Solver.push solver 1;
    Solver.assert_ solver Int.[ x >= int 0 ];
    assert_sat (Solver.check_sat solver ~assumptions:[]);
    assert (
      let v = Solver.get_value solver x in
      Expr.equal v (int 0) );
    Solver.pop solver 1;

    Solver.push solver 1;
    Solver.assert_ solver [ x = int 3 ];
    assert_sat (Solver.check_sat solver ~assumptions:[]);
    assert (
      let v = Solver.get_value solver Int.(x * x) in
      Expr.equal v (int 9) );
    Solver.pop solver 1;

    Solver.push solver 1;
    Solver.assert_ solver Int.[ x >= int 0 || x < int 0 ];
    assert_sat (Solver.check_sat solver ~assumptions:[]);
    (* necessary, otherwise the solver doesn't know x and can't produce a model
       for it *)
    let model = Solver.get_model ~symbols:[ symbol_x ] solver in
    let val_x = Model.evaluate model symbol_x in
    assert (Option.is_some val_x);
    Solver.pop solver 1;

    Solver.assert_ solver [ x = int 5 ];
    assert_sat (Solver.check_sat solver ~assumptions:[]);
    let model = Solver.get_model solver in
    let val_x = Model.evaluate model symbol_x in
    assert (Stdlib.(Some (Value.Int 5) = val_x))
end
