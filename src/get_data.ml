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

let from_json json =
  {
    words = json |> member "word_list" |> to_list |> List.map word_of_json;
    sentences =
      json |> member "sentence_list" |> to_list |> List.map sentence_of_json;
  }
