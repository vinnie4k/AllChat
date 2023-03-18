open Yojson.Basic.Util

exception InvalidWord of string
exception InvalidSentence of string

type part_of_speech =
  | Adjective
  | Noun
  | Verb

type connatation =
  | Positive
  | Neutral
  | Negative

type word = {
  term : string;
  part_of_speech : part_of_speech;
  connatation : connatation;
}

type sentence = {
  sentence : string;
  expected_pos : part_of_speech;
  connatation : connatation;
  internal_representation : string list;
}

type t = {
  words : word list;
  sentences : sentence list;
}
