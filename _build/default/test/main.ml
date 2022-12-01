open OUnit2
open Game
open State

let line = "    |.......|            |.......|  "

let tests =
  "final project test suite"
  >::: [ (* ("Test for check_valid_move" >:: fun _ -> assert_equal
            check_valid_move); *) ]

let _ = run_test_tt_main tests