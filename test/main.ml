open OUnit2
open Allchat
open Get_data
(* open Inputs open Interface open Player *)

let data_dir_prefix = "data" ^ Filename.dir_sep
let test = Yojson.Basic.from_file (data_dir_prefix ^ "test_data.json")
let test_json = from_json test

let int_list_to_string lst =
  List.fold_left (fun acc x -> acc ^ string_of_int x) "" lst

let string_of_string str = str

let calculate_score_test (name : string) (word_repo : Get_data.t)
    (sentence : string) (word_list : string list) (expected_output : string) :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    (int_list_to_string (calculate_score word_repo sentence word_list))
    ~printer:string_of_string

let algorithm_test =
  [
    calculate_score_test
      "gets the scores for Did you see the ___ run across? with [iron, bubbly] \
       in test_data."
      test_json "Did you see the ___ run across?" [ "iron"; "bubbly" ] "10050";
  ]

let suite = "test suite for Allchat" >::: List.flatten [ algorithm_test ]
let _ = run_test_tt_main suite
