(** Representation of word and sentence data.

    This module represents the sentence and word data as provided by one of two
    default json files, or the user's custom file. Get_data pulls data from the
    json when requested and formats/unformats it to be sent between modules. The
    module also contains the score calculating algorithm based on data contained
    in the json file.*)

type t
(** The abstract data type representing the words_repo. *)

exception InvalidSentence of string
(** Raised an exception when an invalid sentence is given. It carries the
    identifier for the unknown sentence. *)

exception OutOfWords
(** Raised an exception when all the words in a file is used. *)

exception OutOfSentences
(** Raised an exception when all the sentences in a file is used. *)

exception InvalidWords of string list
(** Raised an exception when at least one invalid word is given. It carries the
    identifier for the entire list of words. *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the words_repo that [j] represents. Requires: [j] is a
    valid JSON words_repo representation. *)

val get_word : t -> int -> string list
(** [get_word j i] gets [i] amount of words from words_repo [j]. Example:
    [get_word test_data 2] is [\["curious", "bored"\]] because it gets two words
    from the words_repo test_data. When all the words in [j] is used up then
    raise OutOfWords *)

val get_sentence : t -> string
(** [get_sentence j] gets a sentence from words_repo[j]. Example:
    [get_sentence test_data] is ["I was ___ down the street."] because that is a
    sentence from the words_repo test_data. *)

val get_blanks : t -> string -> int
(** [get_blanks j s] gets an int based on the number of blanks ["___"] in the
    string [s] if s is from word_repo[j]. Example:
    [get_blanks "I was ___ down the street."] is [1] because there is 1 blank in
    the sentence. *)

val add_words : t -> string -> string list -> string
(** [add_words j s lst] gets a string by adding the elements from [lst] into the
    blanks of [s] if s is from word_repo[j] and outputs a string. Example:
    [add_words "I was ___ down the street." \["bored"\]] is
    ["I was bored down the street."] *)

val calculate_score : t -> string -> string list -> int list
(** [calculate_score j s lst] gets an int list that represents the score for
    each of the words from word list[lst] using sentence[s] from the
    word_repo[j]. Example:
    [calculate_score test_data "I was ___ down the street." \[“bored”, “iron”\]]
    returns [62, 75] *)

val includes_sentence : t -> string -> bool
(** [includes_sentence j s] gets a bool depending on if the sentence[s] is part
    of the word_repo[j]. Example:
    [includes_sentence test_data "I was ___ down the street."] returns true *)

val includes_word : t -> string -> bool
(** [includes_word j w] gets a bool depending on if the word[w] is part of the
    word_repo[j]. Example: [includes_word test_data "binglebell"] returns false *)
