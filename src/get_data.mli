type t
(** The abstract data type representing the words_repo. *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the words_repo that [j] represents. Requires: [j] is a
    valid JSON words_repo representation. *)

val get_word : t -> int -> string list
(** [get_word j i] gets [i] amount of words from words_repo [j]. Example:
    [get_word test_data 2] is [\["curious", "bored"\]] because it gets two words
    from the words_repo test_data. *)

val get_sentence : t -> string
(** [get_sentence j] gets a sentence from words_repo[j]. Example:
    [get_sentence test_data] is ["I was ___ down the street."] because that is a
    sentence from the words_repo test_data. *)

val get_blanks : string -> int
(** [get_blanks s] gets an int based on the number of blanks ["___"] in the
    string [s]. Example: [get_blanks "I was ___ down the street."] is [1]
    because there is 1 blank in the sentence. *)

val add_words : string -> string list -> string
(** [add_words s lst] gets a string by adding the elements from [lst] into the
    blanks of [s] and outputs a string. Example:
    [add_words "I was ___ down the street." \["bored"\]] is
    ["I was bored down the street."] *)
