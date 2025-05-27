open Test_unit

let () =
  Alcotest.run "Unit_tests"
  @@ Test_bitvector.test :: Test_model.test :: Test_statistics.test
     :: Test_expr.test :: Test_eval.tests
