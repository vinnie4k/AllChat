open OUnit2
open Allchat

let data_dir_prefix = "data" ^ Filename.dir_sep
let test = Yojson.Basic.from_file (data_dir_prefix ^ "test_data.json")
let small = Yojson.Basic.from_file (data_dir_prefix ^ "small_data.json")
let empty = Yojson.Basic.from_file (data_dir_prefix ^ "empty_data.json")
let test_json = Get_data.from_json test
let small_json = Get_data.from_json small
let empty_json = Get_data.from_json empty

let int_list_to_string lst =
  List.fold_left (fun acc x -> acc ^ string_of_int x) "" lst

let string_of_string str = str

let rec check_mems lst1 lst2 =
  match lst1 with
  | [] -> true
  | h :: t -> if List.mem h lst2 then check_mems t lst2 else false

let get_word_test (name : string) (word_repo : Get_data.t) (num_word : int)
    (expected_list : string list) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (check_mems (Get_data.get_word word_repo num_word) expected_list)

let get_word_fail_test (name : string) (word_repo : Get_data.t) (num_word : int)
    (expected_output : exn) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> Get_data.get_word word_repo num_word)

let get_sentence_test (name : string) (word_repo : Get_data.t)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Get_data.includes_sentence word_repo (Get_data.get_sentence word_repo))

let get_sentence_fail_test (name : string) (word_repo : Get_data.t)
    (expected_output : exn) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> Get_data.get_sentence word_repo)

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

let includes_sentence_test (name : string) (word_repo : Get_data.t)
    (sentence : string) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (Get_data.includes_sentence word_repo sentence)

let algorithm_test =
  [
    get_word_test "get a word from small_data" small_json 2
      [ "curious"; "bubbly" ] true;
    get_word_fail_test
      "get an exception because data size isn't large enough from small_data"
      small_json 3 Get_data.OutOfWords;
    get_word_fail_test
      "get an exception because data size isn't large enough from test_data"
      test_json 22 Get_data.OutOfWords;
    get_word_fail_test "get an exception because no data exist in empty_data"
      empty_json 1 Get_data.OutOfWords;
    get_sentence_test "get a sentence from test_data" test_json true;
    get_sentence_test "get a sentence from small_data" small_json true;
    get_sentence_fail_test "get a sentence from small_data" empty_json
      Get_data.OutOfSentences;
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
    calculate_score_fail_test
      "gets the exception for no words with [] as word_list in test_data."
      test_json "How did she ___ the raccoon?" [] (Get_data.InvalidWords []);
    includes_sentence_test
      "check if I was ___ down the street. is a sentence in test_data."
      test_json "I was ___ down the street." true;
    includes_sentence_test
      "check if I hope you ___ into the wall! is a sentence in test_data."
      test_json "I hope you ___ into the wall!" true;
    includes_sentence_test
      "check if The librarian ___ the books. is a sentence in test_data."
      test_json "The librarian ___ the books." true;
    includes_sentence_test
      "check if A FAKE SENTNCE!!!!!!! is a sentence in test_data." test_json
      "A FAKE SENTNCE!!!!!!!" false;
    includes_sentence_test
      "check if Looks like a real sentence. is a sentence in test_data."
      test_json "Looks like a real sentence." false;
    includes_sentence_test
      "check if The play was so much ___ today! is a sentence in test_data."
      test_json "The play was so much ___ today!" true;
    includes_sentence_test
      "check if Can ___ please quiet down! is a sentence in test_data."
      test_json "Can ___ please quiet down!" true;
    includes_sentence_test
      "check if Can ___ please quiet down. is a sentence in test_data."
      test_json "Can ___ please quiet down." false;
  ]

let suite = "test suite for Allchat" >::: List.flatten [ algorithm_test ]
let _ = run_test_tt_main suite
