open Yojson.Basic.Util

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

(* currently this function could choose the same word twice!!!!! *)

(** helper function for get_word *)
let str_lst = []

let rec get_word_helper repo int str_lst =
  if int = 0 then str_lst
  else
    let word = List.length repo.words |> Random.int |> List.nth repo.words in
    get_word_helper repo (int - 1) (word.term :: str_lst)

(** get_word function *)
let rec get_word repo int = get_word_helper repo int str_lst

(** get_sentence function *)
let get_sentence repo =
  let sentence =
    List.length repo.sentences |> Random.int |> List.nth repo.sentences
  in
  List.cons sentence []
