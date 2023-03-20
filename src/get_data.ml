open Yojson.Basic.Util

exception UnknownWord of string
exception UnknownSentence of string

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

(** helper function for get_word *)
let rec get_word_helper repo int str_lst =
  if int = 0 then str_lst
  else
    let word = List.length repo.words |> Random.int |> List.nth repo.words in
    get_word_helper repo (int - 1) (word.term :: str_lst)

(** get_word function *)
(** currently this function could choose the same word twice!!!!! *)
let rec get_word repo int = get_word_helper repo int []

(** get_sentence function *)
let get_sentence repo =
  let sentence =
    List.length repo.sentences |> Random.int |> List.nth repo.sentences
  in
  sentence.sentence

(** helper function for get_blanks *)
let rec get_blanks_helper lst sentence = 
  match lst with
  | [] -> raise (UnknownSentence sentence)
  | h :: t -> if h.sentence = sentence 
                then List.length h.internal_representation - 1 
                else get_blanks_helper t sentence

(** get_blanks function *)
let get_blanks repo sentence =
  get_blanks_helper repo.sentences sentence

(** helper function for add_words *)
let rec alternate_lst lst1 lst2 lst3 = 
  match lst1 with
  | [] -> lst3
  | h :: t -> alternate_lst lst2 t (h :: lst3)
  let rec add_words_helper lst sentence word =
  match lst with
  | [] -> raise (UnknownSentence sentence)
  | h :: t -> if h.sentence = sentence 
                then alternate_lst h.internal_representation word [] |> List.rev
                else add_words_helper t sentence word

(** add_words function *)
let add_words repo sentence word =
  add_words_helper repo.sentences sentence word |> List.hd
