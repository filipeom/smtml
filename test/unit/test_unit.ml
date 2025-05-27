let () =
  Alcotest.run "Unit_tests"
  @@ Test_expr.test
     :: List.flatten
          [ Test_model.testsuite
          ; Test_statistics.testsuite
          ; Test_bitvector.testsuite
          ; Test_eval.testsuite
          ]
