open Alcotest
open Smtml
module Map = Statistics.Map

let entry = testable Statistics.pp_entry Statistics.equal_entry

let testsuite : unit Alcotest.test list =
  [ ( "Statistics"
    , [ ( test_case "test_merge" `Quick @@ fun _ ->
          let s1 = Map.empty |> Map.add "time" (`Float 10.0) in
          let s2 = Map.empty |> Map.add "time" (`Float 20.0) in
          check (option entry) "10.0 + 20.0 is 30.0 "
            (Some (`Float 30.0))
            (Statistics.merge s1 s2 |> Map.find_opt "time") )
      ] )
  ]
