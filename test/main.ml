open OUnit2
open Allchat

let data_dir_prefix = "data" ^ Filename.dir_sep
let test = Yojson.Basic.from_file (data_dir_prefix ^ "test_data.json")
let test_json = Get_data.from_json test

let int_list_to_string lst =
  List.fold_left (fun acc x -> acc ^ string_of_int x) "" lst

let string_of_string str = str

let calculate_score_test (name : string) (word_repo : Get_data.t)
    (sentence : string) (word_list : string list) (expected_output : string) :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    (int_list_to_string (Get_data.calculate_score word_repo sentence word_list))
    ~printer:string_of_string

let calculate_score_fail_test (name : string) (word_repo : Get_data.t)
    (sentence : string) (word_list : string list) (expected_output : exn) : test
    =
  name >:: fun _ ->
  assert_raises expected_output (fun () ->
      Get_data.calculate_score word_repo sentence word_list)

let algorithm_test =
  [
    calculate_score_test
      "gets the scores for Did you see the ___ run across? with [iron, bubbly] \
       in test_data."
      test_json "Did you see the ___ run across?" [ "iron"; "bubbly" ] "10050";
    calculate_score_test
      "gets the scores for How did she ___ the raccoon? with [running, braids, \
       curious] in test_data."
      test_json "How did she ___ the raccoon?"
      [ "running"; "braids"; "curious" ]
      "1007550";
    calculate_score_fail_test
      "gets the exception for How did she ___ the Donkey Kong? with [running, \
       braids, curious] in test_data."
      test_json "How did she ___ the Donkey Kong?"
      [ "running"; "braids"; "curious" ]
      (Get_data.InvalidSentence "How did she ___ the Donkey Kong?");
    calculate_score_fail_test
      "gets the exception for How did she ___ the raccoon? with [BAHAHA, \
       braids, curious] in test_data."
      test_json "How did she ___ the raccoon?"
      [ "BAHAHA"; "braids"; "curious" ]
      (Get_data.InvalidWords [ "BAHAHA"; "braids"; "curious" ]);
    calculate_score_fail_test
      "gets the exception for Fake sentence with [BAHAHA, braids, curious] in \
       test_data."
      test_json "Fake sentence"
      [ "BAHAHA"; "braids"; "curious" ]
      (Get_data.InvalidWords [ "BAHAHA"; "braids"; "curious" ]);
  ]

let suite = "test suite for Allchat" >::: List.flatten [ algorithm_test ]
let _ = run_test_tt_main suite
