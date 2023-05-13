open Yojson.Basic.Util

exception InvalidSentence of string
exception OutOfWords
exception OutOfSentences
exception InvalidWords of string list

type part_of_speech =
  | Adjective
  | Noun
  | Verb

type connotation =
  | Positive
  | Neutral
  | Negative

type word = {
  term : string;
  part_of_speech : part_of_speech;
  connotation : connotation;
}

type sentence = {
  sentence : string;
  expected_pos : part_of_speech;
  connotation : connotation;
  internal_representation : string list;
}

type t = {
  words : word list;
  sentences : sentence list;
}

(** helper functions for from_json *)
let match_pos str =
  match str with
  | v ->
      if v = "adjective" then Adjective else if v = "noun" then Noun else Verb

let match_connotation str =
  match str with
  | v ->
      if v = "positive" then Positive
      else if v = "neutral" then Neutral
      else Negative

let word_of_json json =
  {
    term = json |> member "word" |> to_string;
    part_of_speech = json |> member "part_of_speech" |> to_string |> match_pos;
    connotation = json |> member "connotation" |> to_string |> match_connotation;
  }

let sentence_of_json json =
  {
    sentence = json |> member "sentence" |> to_string;
    expected_pos = json |> member "expected_pos" |> to_string |> match_pos;
    connotation = json |> member "connotation" |> to_string |> match_connotation;
    internal_representation =
      json |> member "internal_representation" |> to_list |> List.map to_string;
  }

(** from_json function *)
let from_json json =
  {
    words = json |> member "word_list" |> to_list |> List.map word_of_json;
    sentences =
      json |> member "sentence_list" |> to_list |> List.map sentence_of_json;
  }

type base = { mutable used : int list }

(* helper function for get_word *)
let word_base = { used = [] }

(* debugging function that I might need *)
(* let rec print_words lst = match lst with | [] -> print_string "done" | h :: t
   -> print_words t; h |> string_of_int |> print_string *)

let still_words repo = List.length repo.words - List.length word_base.used > 0

let rec check_word_dup int upper =
  if List.mem int word_base.used then check_word_dup (Random.int upper) upper
  else int

let rec get_word_helper repo int str_lst =
  if int = 0 then str_lst
  else if still_words repo then (
    let size = List.length repo.words in
    let this_int = check_word_dup (Random.int size) size in
    let word = List.nth repo.words this_int in
    word_base.used <- this_int :: word_base.used;
    get_word_helper repo (int - 1) (word.term :: str_lst))
  else raise OutOfWords
(* shouldn't get duplicates now, but need to test *)
(* let word = List.length repo.words |> Random.int |> List.nth repo.words in
   get_word_helper repo (int - 1) (word.term :: str_lst) *)

(** get_word function *)
let get_word repo int = get_word_helper repo int []

(* helper function for include_words *)
let rec check_word json word =
  match json with
  | [] -> false
  | h :: t -> if h.term = word then true else check_word t word

(** includes_word function *)
let includes_word repo word =
  let json = repo.words in
  check_word json word

(* helper function for get_sentence *)
let sentence_base = { used = [] }

let still_sentences repo =
  List.length repo.sentences - List.length sentence_base.used > 0

let check_sentence_dup int upper =
  if List.mem int sentence_base.used then
    check_word_dup (Random.int upper) upper
  else int

(** get_sentence function *)
let get_sentence repo =
  if still_sentences repo then (
    let size = List.length repo.sentences in
    let this_int = check_sentence_dup (Random.int size) size in
    let sentence = List.nth repo.sentences this_int in
    sentence_base.used <- this_int :: sentence_base.used;
    sentence.sentence)
  else raise OutOfSentences
(* let sentence = List.length repo.sentences |> Random.int |> List.nth
   repo.sentences in sentence.sentence *)

(** helper function for includes_sentence *)
let rec check_sentence json sentence =
  match json with
  | [] -> false
  | h :: t -> if h.sentence = sentence then true else check_sentence t sentence

(** includes_sentence function *)
let includes_sentence repo sentence =
  let json = repo.sentences in
  check_sentence json sentence

(** helper function for get_blanks *)
let rec get_blanks_helper lst sentence =
  match lst with
  | [] -> raise (InvalidSentence sentence)
  | h :: t ->
      if h.sentence = sentence then List.length h.internal_representation - 1
      else get_blanks_helper t sentence

(** get_blanks function *)
let get_blanks repo sentence = get_blanks_helper repo.sentences sentence

(** helper function for add_words *)
let rec alternate_lst lst1 lst2 lst3 =
  match lst1 with
  | [] -> lst3
  | h :: t -> alternate_lst lst2 t (h :: lst3)

let rec add_words_helper lst sentence word =
  match lst with
  | [] -> raise (InvalidSentence sentence)
  | h :: t ->
      if h.sentence = sentence then
        alternate_lst h.internal_representation word [] |> List.rev
      else add_words_helper t sentence word

(** add_words function *)
let add_words repo sentence word =
  add_words_helper repo.sentences sentence word |> List.hd

(* need to check both word and sentence are part of the word_repo before
   calculating the score *)
let rec wordin_word_repo words word =
  match words with
  | [] -> false
  | h :: t -> if h.term = word then true else wordin_word_repo t word

let rec check_all_words repo lst =
  match lst with
  | [] -> true
  | h :: t ->
      if wordin_word_repo repo.words h then check_all_words repo t else false

let rec sentencein_word_repo sentences sentence =
  match sentences with
  | [] -> false
  | h :: t ->
      if h.sentence = sentence then true else sentencein_word_repo t sentence

(** calculations for a word *)
let score_matching sen_pos word_pos sen_con word_con =
  if sen_pos = word_pos && sen_con = word_con then 0
  else if sen_pos <> word_pos && sen_con = word_con then 25
  else if sen_pos = word_pos && sen_con <> word_con then 25
  else 50

(** return the calculations for a word *)
let calculate repo sentence (word : string) =
  let sentence_prop =
    List.filter (fun x -> x.sentence = sentence) repo.sentences
  in
  let word_prop = List.filter (fun x -> x.term = word) repo.words in
  score_matching (List.hd sentence_prop).expected_pos
    (List.hd word_prop).part_of_speech (List.hd sentence_prop).connotation
    (List.hd word_prop).connotation

(** gets the list of all the scores *)
let rec calculate_score_helper repo sentence (words : string list) scores =
  if words = [] && scores = [] then raise (InvalidWords words)
  else
    match words with
    | [] -> scores
    | h :: t ->
        calculate_score_helper repo sentence t
          ((100 - calculate repo sentence h) :: scores)

(** calculate_score function *)
let calculate_score repo sentence (words : string list) =
  if Bool.not (check_all_words repo words) then raise (InvalidWords words)
  else if Bool.not (sentencein_word_repo repo.sentences sentence) then
    raise (InvalidSentence sentence)
  else calculate_score_helper repo sentence (List.rev words) []
(* change calculate_score to string list list so can have multiple inputs *)
