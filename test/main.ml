open OUnit2
open Allchat

(** Test plan: for testing, most of the backend functions were tested using
    OUnit tests, while the ones that needed some kind of user input such as the
    functions in interface were manually tested. The OUnit tests consists of
    get_data.ml, game_state.ml, and player.ml. The manually tested ones are the
    functions in interface.ml and bin/main.ml. The backend functions that were
    tested is all glass box tests because the person that wrote the functions
    was the same person that tested, so they already knew what output to expect.
    This made it easy for them to locate the errors because they knew where the
    error is, whenever a function isn't working as intended. We also tried to
    practice code coverage when testing the functions to further ensure that the
    functions will work correctly. Because the user has to input certain
    information, we had to manually test those, so we did those by playing the
    game ourselves and writing inputs that we think the user might say and
    checking if the game will act how we think it would. This method of testing
    should be robust because the backend functions are tested to output
    correctly, so when the frontend calls them they should work correctly. By
    testing the frontend, we also ensure that the potential inputs that the user
    gives are also tested for. Obviously we cannot predict all inputs of the
    user, but our functions have failsafes that catch invalid inputs. *)

let data_dir_prefix = "data" ^ Filename.dir_sep
let test = Yojson.Basic.from_file (data_dir_prefix ^ "test_data.json")
let small = Yojson.Basic.from_file (data_dir_prefix ^ "small_data.json")
let empty = Yojson.Basic.from_file (data_dir_prefix ^ "empty_data.json")
let toxic = Yojson.Basic.from_file (data_dir_prefix ^ "toxic_data.json")
let wholesome = Yojson.Basic.from_file (data_dir_prefix ^ "wholesome_data.json")
let test_json = Get_data.from_json test
let small_json = Get_data.from_json small
let empty_json = Get_data.from_json empty
let toxic_json = Get_data.from_json toxic
let wholesome_json = Get_data.from_json wholesome

let int_list_to_string lst =
  List.fold_left (fun acc x -> acc ^ string_of_int x) "" lst

let string_of_string str = str

let rec check_mems lst1 lst2 =
  match lst1 with
  | [] -> true
  | h :: t -> if List.mem h lst2 then check_mems t lst2 else false

(*get_data test suite*)
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

let get_blanks_test (name : string) (word_repo : Get_data.t) (sentence : string)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (Get_data.get_blanks word_repo sentence)

let get_blanks_fail_test (name : string) (word_repo : Get_data.t)
    (sentence : string) (expected_output : exn) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () ->
      Get_data.get_blanks word_repo sentence)

let add_words_test (name : string) (word_repo : Get_data.t) (sentence : string)
    (word_list : string list) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Get_data.add_words word_repo sentence word_list)
    ~printer:string_of_string

let add_words_fail_test (name : string) (word_repo : Get_data.t)
    (sentence : string) (word_list : string list) (expected_output : exn) : test
    =
  name >:: fun _ ->
  assert_raises expected_output (fun () ->
      Get_data.add_words word_repo sentence word_list)

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

let includes_word_test (name : string) (word_repo : Get_data.t) (word : string)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (Get_data.includes_word word_repo word)

let algorithm_test =
  [
    get_word_test "get a word from small_data" small_json 2
      [ "curious"; "bubbly" ] true;
    get_word_test "get a word from small_data" small_json 2
      [ "bubbly"; "curious" ] true;
    get_word_fail_test
      "get an exception because data size isn't large enough from small_data"
      small_json 3 Get_data.OutOfWords;
    get_word_fail_test
      "get an exception because data size isn't large enough from small_data"
      small_json 5 Get_data.OutOfWords;
    get_word_fail_test
      "get an exception because data size isn't large enough from small_data"
      small_json 100 Get_data.OutOfWords;
    get_word_fail_test
      "get an exception because data size isn't large enough from test_data"
      test_json 300 Get_data.OutOfWords;
    get_word_fail_test
      "get an exception because data size isn't large enough from toxic_data"
      toxic_json 200 Get_data.OutOfWords;
    get_word_fail_test
      "get an exception because data size isn't large enough from \
       wholesome_data"
      wholesome_json 500 Get_data.OutOfWords;
    get_word_fail_test "get an exception because no data exist in empty_data"
      empty_json 1 Get_data.OutOfWords;
    get_sentence_test "get a sentence from small_data" small_json true;
    get_sentence_test "get a sentence from test_data" test_json true;
    get_sentence_fail_test "get a sentence from empty_data" empty_json
      Get_data.OutOfSentences;
    get_blanks_test
      "gets the amount of blanks in a Did you see the ___ run across? from \
       test_data"
      test_json "Did you see the ___ run across?" 1;
    get_blanks_test
      "gets the amount of blanks in a How did she ___ the raccoon? from \
       test_data"
      test_json "How did she ___ the raccoon?" 1;
    get_blanks_test
      "gets the amount of blanks in a That ice cream was so ___! from test_data"
      test_json "That ice cream was so ___!" 1;
    get_blanks_test
      "gets the amount of blanks in a The play was so much ___ today! from \
       test_data"
      test_json "The play was so much ___ today!" 1;
    get_blanks_test
      "gets the amount of blanks in a I was ___ down the street. from \
       small_json"
      small_json "I was ___ down the street." 1;
    get_blanks_fail_test
      "gets the amount of blanks in a The company was so much ___ today! from \
       small_json"
      small_json "Willard Straight company was so much ___ today!"
      (Get_data.InvalidSentence
         "Willard Straight company was so much ___ today!");
    get_blanks_fail_test
      "gets the amount of blanks in a The company was so much ___ today! from \
       test_data"
      test_json "The company was so much ___ today!"
      (Get_data.InvalidSentence "The company was so much ___ today!");
    get_blanks_fail_test
      "gets the amount of blanks in a I WANT TO STUDY FOR ALGO RIGHT! NOW from \
       test_data"
      test_json "I WANT TO STUDY FOR ALGO RIGHT NOW!"
      (Get_data.InvalidSentence "I WANT TO STUDY FOR ALGO RIGHT NOW!");
    add_words_test
      "adding the word bubbly to the sentence Did you see the ___ run across? \
       from test_json"
      test_json "Did you see the ___ run across?" [ "bubbly" ]
      "Did you see the bubbly run across?";
    add_words_test
      "adding the word adventurous to the sentence I hope you ___ into the \
       wall! from test_json"
      test_json "I hope you ___ into the wall!" [ "adventurous" ]
      "I hope you adventurous into the wall!";
    add_words_test
      "adding the word dance to the sentence That ice cream was so ___! from \
       test_json"
      test_json "That ice cream was so ___!" [ "dance" ]
      "That ice cream was so dance!";
    add_words_test
      "adding the word gentle to the sentence Crap, I ___ my homework again. \
       from test_json"
      test_json "Crap, I ___ my homework again." [ "gentle" ]
      "Crap, I gentle my homework again.";
    add_words_test
      "adding the word playing to the sentence The librarian ___ the books. \
       from test_json"
      test_json "The librarian ___ the books." [ "playing" ]
      "The librarian playing the books.";
    add_words_test
      "adding the word harsh to the sentence There is a piece of ___ on the \
       books. from test_json"
      test_json "There is a piece of ___ on the books." [ "harsh" ]
      "There is a piece of harsh on the books.";
    add_words_test
      "adding the word impatient to the sentence In a full ___ it feels so \
       empty. from test_json"
      test_json "In a full ___ it feels so empty." [ "impatient" ]
      "In a full impatient it feels so empty.";
    add_words_fail_test
      "get an exception when attempting to add the word bulldog to the \
       sentence Did you see the ___ run across? from test_json"
      test_json "Did you see the ___ run across?" [ "bulldog" ]
      (Get_data.InvalidWords [ "bulldog" ]);
    add_words_fail_test
      "get an exception when attempting to add the word christmas to the \
       sentence Fake sentence? from test_json"
      test_json "Fake sentence?" [ "christmas" ]
      (Get_data.InvalidWords [ "christmas" ]);
    add_words_fail_test
      "get an exception when attempting to add the word bubbly to the sentence \
       Invalid sentence did you see the ___ run across? from test_json"
      test_json "Invalid sentence did you see the ___ run across?" [ "bubbly" ]
      (Get_data.InvalidSentence
         "Invalid sentence did you see the ___ run across?");
    add_words_fail_test
      "get an exception when attempting to add the word bubbly to the sentence \
       Fake sentence? from test_json"
      test_json "Fake sentence?" [ "bubbly" ]
      (Get_data.InvalidSentence "Fake sentence?");
    calculate_score_test
      "gets the scores for Did you see the ___ run across? with [iron, bubbly] \
       in test_data."
      test_json "Did you see the ___ run across?" [ "iron"; "bubbly" ] "10050";
    calculate_score_test
      "gets the scores for The librarian ___ the books. with [careless, \
       impatient] in test_data."
      test_json "The librarian ___ the books."
      [ "careless"; "impatient" ]
      "5050";
    calculate_score_test
      "gets the scores for Talk about the ___ nose I've ever seen. with [lazy] \
       in test_data."
      test_json "Talk about the ___ nose I've ever seen." [ "lazy" ] "100";
    calculate_score_test
      "gets the scores for The librarian ___ the books. with [careless, \
       impatient, imitate] in test_data."
      test_json "The librarian ___ the books."
      [ "careless"; "impatient"; "imitate" ]
      "505075";
    calculate_score_test
      "gets the scores for How did she ___ the raccoon? with [running, braids, \
       curious] in test_data."
      test_json "How did she ___ the raccoon?"
      [ "running"; "braids"; "curious" ]
      "1007550";
    calculate_score_test
      "gets the scores for The librarian ___ the books. with [careless, \
       impatient, imitate, imitate] in test_data."
      test_json "The librarian ___ the books."
      [ "careless"; "impatient"; "imitate"; "imitate" ]
      "50507575";
    calculate_score_test
      "gets the scores for The librarian ___ the books. with [careless, \
       impatient, imitate, imitate, fool] in test_data."
      test_json "The librarian ___ the books."
      [ "careless"; "impatient"; "imitate"; "imitate"; "fool" ]
      "5050757550";
    calculate_score_test
      "gets the scores for Bruh this bird looks ___ running like that. with \
       [victorious, rude, meat, marriage] in test_data."
      test_json "Bruh this bird looks ___ running like that."
      [ "victorious"; "rude"; "meat"; "marriage" ]
      "75757550";
    calculate_score_test
      "gets the scores for How did she ___ the raccoon? with [running, braids, \
       curious, running, braids, curious] in test_data."
      test_json "How did she ___ the raccoon?"
      [ "running"; "braids"; "curious"; "running"; "braids"; "curious" ]
      "10075501007550";
    calculate_score_test
      "gets the scores for The librarian ___ the books. with [careless, \
       impatient, imitate, imitate, fool, running] in test_data."
      test_json "The librarian ___ the books."
      [ "careless"; "impatient"; "imitate"; "imitate"; "fool"; "running" ]
      "5050757550100";
    calculate_score_test
      "gets the scores for Cornell is the ___ university. with [sing, \
       impatient] in toxic_data."
      toxic_json "Cornell is the ___ university." [ "sing"; "impatient" ] "5075";
    calculate_score_test
      "gets the scores for CS4820 is the most ___ course. with [careless, \
       impatient] in toxic_data."
      toxic_json "CS4820 is the most ___ course."
      [ "careless"; "impatient" ]
      "100100";
    calculate_score_test
      "gets the scores for Doctor, why does my ___ hurt? with [boyfriend] in \
       toxic_data."
      toxic_json "Doctor, why does my ___ hurt?" [ "boyfriend" ] "75";
    calculate_score_test
      "gets the scores for Bruh this bird looks ___ running like that. with \
       [honey, brave, deceitful] in toxic_data."
      toxic_json "Bruh this bird looks ___ running like that."
      [ "honey"; "brave"; "deceitful" ]
      "5075100";
    calculate_score_fail_test
      "gets the exception for How did she ___ the samurai sword? with \
       [running, braids, curious] in test_data."
      test_json "How did she ___ the samurai sword?"
      [ "running"; "braids"; "curious" ]
      (Get_data.InvalidSentence "How did she ___ the samurai sword?");
    calculate_score_fail_test
      "gets the exception for How did she ___ the doesn't make sense? with \
       [running, braids, curious] in test_data."
      test_json "How did she ___ the doesn't make sense?"
      [ "running"; "braids"; "curious" ]
      (Get_data.InvalidSentence "How did she ___ the doesn't make sense?");
    calculate_score_fail_test
      "gets the exception for In the void did she ___ the doesn't make sense? \
       with [running, braids, curious] in test_data."
      test_json "In the void did she ___ the doesn't make sense?"
      [ "running"; "braids"; "curious" ]
      (Get_data.InvalidSentence
         "In the void did she ___ the doesn't make sense?");
    calculate_score_fail_test
      "gets the exception for In the void did she ___ the doesn't make sense? \
       with [bubbly, iron, curious] in test_data."
      test_json "In the void did she ___ the doesn't make sense?"
      [ "bubbly"; "iron"; "curious" ]
      (Get_data.InvalidSentence
         "In the void did she ___ the doesn't make sense?");
    calculate_score_fail_test
      "gets the exception for On the trampoline did she ___ the doesn't make \
       sense? with [curious, bubbly] in small_data."
      small_json "On the trampoline did she ___ the doesn't make sense?"
      [ "curious"; "bubbly" ]
      (Get_data.InvalidSentence
         "On the trampoline did she ___ the doesn't make sense?");
    calculate_score_fail_test
      "gets the exception for How did she ___ the raccoon? with [BAHAHA, \
       braids, curious] in test_data."
      test_json "How did she ___ the raccoon?"
      [ "BAHAHA"; "braids"; "curious" ]
      (Get_data.InvalidWords [ "BAHAHA"; "braids"; "curious" ]);
    calculate_score_fail_test
      "gets the exception for How did she ___ the raccoon? with [BAHAHA, \
       braids, curious] in empty_data."
      empty_json "How did she ___ the raccoon?"
      [ "BAHAHA"; "braids"; "curious" ]
      (Get_data.InvalidWords [ "BAHAHA"; "braids"; "curious" ]);
    calculate_score_fail_test
      "gets the exception for How did she ___ the raccoon? with [] in \
       test_data."
      test_json "How did she ___ the raccoon?" [] (Get_data.InvalidWords []);
    calculate_score_fail_test
      "gets the exception for I was ___ down the street. with [] in small_data."
      small_json "I was ___ down the street." [] (Get_data.InvalidWords []);
    calculate_score_fail_test
      "gets the exception for Fake sentence with [BAHAHA, braids, curious] in \
       test_data."
      test_json "Fake sentence"
      [ "BAHAHA"; "braids"; "curious" ]
      (Get_data.InvalidWords [ "BAHAHA"; "braids"; "curious" ]);
    calculate_score_fail_test
      "gets the exception for Fake sentence with [not, one, thing] in \
       test_data."
      test_json "Fake sentence" [ "not"; "one"; "thing" ]
      (Get_data.InvalidWords [ "not"; "one"; "thing" ]);
    calculate_score_fail_test
      "gets the exception for How did she ___ the raccoon? with [another, \
       completely, unreal] in test_data."
      test_json "How did she ___ the raccoon?"
      [ "another"; "completely"; "unreal" ]
      (Get_data.InvalidWords [ "another"; "completely"; "unreal" ]);
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
      "check if I hope you ___ into the wall. is a sentence in test_data. This \
       sentence is actually in test_data but the punctuation is different."
      test_json "I hope you ___ into the wall." false;
    includes_sentence_test
      "check if The librarian ___ the books. is a sentence in toxic_data."
      toxic_json "The librarian ___ the books." true;
    includes_sentence_test
      "check if A FAKE SENTNCE!!!!!!! is a sentence in toxic_data." toxic_json
      "A FAKE SENTNCE!!!!!!!" false;
    includes_sentence_test
      "check if Looks like a real sentence. is a sentence in toxic_data."
      toxic_json "Looks like a real sentence." false;
    includes_sentence_test
      "check if The play was so much ___ today! is a sentence in toxic_data."
      toxic_json "The play was so much ___ today!" true;
    includes_sentence_test
      "check if I hate this assignment. is a sentence in wholesome_data."
      wholesome_json "I hate this assignment." false;
    includes_sentence_test
      "check if Can ___ please quiet down! is a sentence in wholesome_data."
      wholesome_json "Can ___ please quiet down!" true;
    includes_sentence_test
      "check if Can ___ please quiet down. is a sentence in wholesome_data."
      wholesome_json "Can ___ please quiet down." false;
    includes_word_test "check if curious is a word in test_data." test_json
      "curious" true;
    includes_word_test "check if harsh is a word in test_data." test_json
      "harsh" true;
    includes_word_test "check if dark is a word in test_data." test_json "dark"
      true;
    includes_word_test "check if joyous is a word in test_data." test_json
      "joyous" true;
    includes_word_test "check if angry is a word in test_data." test_json
      "angry" true;
    includes_word_test "check if playing is a word in test_data." test_json
      "playing" true;
    includes_word_test "check if interested is a word in test_data." test_json
      "interested" true;
    includes_word_test "check if bruh is a word in toxic_data." toxic_json
      "bruh" false;
    includes_word_test "check if binglebell is a word in toxic_data." toxic_json
      "binglebell" false;
    includes_word_test "check if looong is a word in toxic_data." toxic_json
      "looong" false;
    includes_word_test "check if birthday is a word in toxic_data." toxic_json
      "birthday" true;
    includes_word_test "check if braids is a word in toxic_data." toxic_json
      "braids" true;
    includes_word_test "check if dance is a word in toxic_data." toxic_json
      "dance" true;
    includes_word_test "check if history is a word in wholesome_data."
      wholesome_json "history" true;
    includes_word_test "check if aloof is a word in wholesome_data."
      wholesome_json "aloof" true;
    includes_word_test "check if bruh is a word in wholesome_data."
      wholesome_json "bruh" false;
    includes_word_test "check if idiot is a word in wholesome_data."
      wholesome_json "idiot" false;
    includes_word_test "check if dishonesty is a word in wholesome_data."
      wholesome_json "dishonesty" true;
  ]

(*player test suite*)
let new_player_liam = Player.new_player "liam"
let new_player_1 = Player.new_player "12345"
let new_player_charlie = Player.new_player "charlie"
let new_player_vin = Player.new_player "vin"
let new_player_enjie = Player.new_player "enjie"
let new_player_rando1 = Player.new_player "rando1"
let new_player_rando2 = Player.new_player "rando2"
let new_player_rando3 = Player.new_player "rando3"

let new_player_test (name : string) (n : string) (expected_output : Player.t) :
    test =
  name >:: fun _ -> assert_equal expected_output (Player.new_player n)

let get_player_name_test (name : string) (player : Player.t)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Player.get_player_name player)

let get_player_score_test (name : string) (player : Player.t)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (Player.get_player_score player)

let get_player_word_list_test (name : string) (player : Player.t)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (Player.get_player_word_list player)

let update_player_score_test (name : string) (player : Player.t) (p : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  let () = Player.update_score player p in
  assert_equal expected_output (Player.get_player_score player)

let update_player_score_twice_test (name : string) (player : Player.t) (p : int)
    (p2 : int) (expected_output : int) : test =
  name >:: fun _ ->
  let () = Player.update_score player p in
  let () = Player.update_score player p2 in
  assert_equal expected_output
    (Player.get_player_score player)
    ~printer:string_of_int

let update_player_score_three_test (name : string) (player : Player.t) (p : int)
    (p2 : int) (p3 : int) (expected_output : int) : test =
  name >:: fun _ ->
  let () = Player.update_score player p in
  let () = Player.update_score player p2 in
  let () = Player.update_score player p3 in
  assert_equal expected_output
    (Player.get_player_score player)
    ~printer:string_of_int

let player_test =
  [
    new_player_test "check initialize new player" "liam" new_player_liam;
    new_player_test "check initialize new player" "12345" new_player_1;
    get_player_name_test "get player name liam" new_player_liam "liam";
    get_player_name_test "get player name integer" new_player_1 "12345";
    get_player_score_test "get player score liam" new_player_liam 0;
    get_player_score_test "get player score integer" new_player_1 0;
    get_player_word_list_test "word list of initial player should be empty"
      new_player_liam [];
    update_player_score_test "adding 5 points to liam" new_player_liam 5 5;
    update_player_score_twice_test "adding 5 and 5 is 10 pts" new_player_rando3
      5 5 10;
    update_player_score_twice_test "adding 5 and 0 is 5 pts" new_player_1 5 0 5;
    update_player_score_twice_test "adding 0 and 0 is 0 pts" new_player_rando1 0
      0 0;
    update_player_score_twice_test "adding 7 and 7 is 14 pts" new_player_charlie
      7 7 14;
    update_player_score_twice_test "adding 0 and -3 is -3 pts" new_player_enjie
      0 (-3) (-3);
    update_player_score_twice_test "adding 1000 and 3000 is 4000 pts"
      new_player_vin 1000 3000 4000;
    update_player_score_three_test "adding 1000 and 3000 and 3000 is 4000 pts"
      new_player_rando1 1000 3000 3000 7000;
    update_player_score_three_test "adding 1 and 1 and 1 is 3 pts"
      new_player_rando1 1 1 1 3;
    update_player_score_three_test "adding 2000 and -4000 and 1000 is -1000 pts"
      new_player_rando2 2000 (-4000) 1000 (-1000);
  ]

(*interface test suite*)
let player_list_cool =
  [ new_player_liam; new_player_charlie; new_player_enjie; new_player_vin ]

let one_player_list = [ new_player_vin ]

let fetch_names_test (name : string) (player_lst : Player.t list)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (Interface.fetch_player_names player_lst)

let names_separated_test (name : string) (name_lst : string list)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (Interface.names_separated name_lst)
    ~printer:(fun x -> x)

let create_game_mode_test (name : string) (input : string)
    (expected_output : Game_state.game_mode) : test =
  name >:: fun _ ->
  assert_equal expected_output (Interface.create_game_mode input)
    ~printer:(fun x ->
      match x with
      | Game_state.Wholesome -> "Wholesome"
      | Game_state.Toxic -> "Toxic")

let create_num_players_test (name : string) (input : string)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Interface.create_num_players input)
    ~printer:string_of_int

(* let create_format_word_bank_test (name : string) (input : string list)
   (expected_output : string) : test = name >:: fun _ -> assert_equal
   expected_output (Interface.format_word_bank input) ~printer:(fun x -> x) *)

let interface_test =
  [
    fetch_names_test "testing list of players Liam, Charlie, Enjie, Vin"
      player_list_cool
      [ "Liam"; "Charlie"; "Enjie"; "Vin" ];
    fetch_names_test "testing one player list" one_player_list [ "Vin" ];
    fetch_names_test "testing empty list" [] [];
    names_separated_test "testing separated 4 names list"
      [ "Liam"; "Charlie"; "Enjie"; "Vin" ]
      "Liam, Charlie, Enjie, Vin";
    names_separated_test "testing separating empty list" [] "";
    names_separated_test "testing separated 2 names list" [ "Liam"; "Charlie" ]
      "Liam and Charlie";
    names_separated_test "testing separated 1 names list" [ "Liam" ] "Charlie";
    create_game_mode_test "testing game mode input wholesome" "wholesome"
      Game_state.Wholesome;
    create_game_mode_test "testing game mode input w" "w" Game_state.Wholesome;
    create_game_mode_test "testing game mode input W" "W" Game_state.Wholesome;
    create_game_mode_test "testing game mode input Wholesome" "Wholesome"
      Game_state.Wholesome;
    create_game_mode_test "testing game mode input WHOLESOME" "WHOLESOME"
      Game_state.Wholesome;
    create_game_mode_test "testing game mode input whOlEsome" "whOlEsome"
      Game_state.Wholesome;
    create_game_mode_test "testing game mode input toxic" "toxic"
      Game_state.Toxic;
    create_game_mode_test "testing game mode input t" "t" Game_state.Toxic;
    create_game_mode_test "testing game mode input T" "T" Game_state.Toxic;
    create_game_mode_test "testing game mode input TOXIC" "TOXIC"
      Game_state.Toxic;
    create_game_mode_test "testing game mode input Toxic" "Toxic"
      Game_state.Toxic;
    create_game_mode_test "testing game mode input special" "shut the fuck up"
      Game_state.Toxic;
    create_num_players_test "testing num players input" "1" 1;
    create_num_players_test "testing num players input" "99999" 99999;
  ]
(*game state test suite*)

(* let new_game_data_test (name : string) (g_mode : Game_state.game_mode) (num_p
   : int) (name_array : Player.t array) (expected_output : Game_state.game_data)
   : test = name >:: fun _ -> assert_equal expected_output (let n =
   Game_state.initialize_game g_mode num_p name_array) *)

let suite =
  "test suite for Allchat"
  >::: List.flatten [ algorithm_test; player_test; interface_test ]

let _ = run_test_tt_main suite
