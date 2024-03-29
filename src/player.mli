(** Representation of dynamic player state.

    This module represents the player of the game. It consists of building
    blocks for creating a player, such as their name, score, and word list. In
    addition, this module provides ways to access these variables as well as
    ways to modify/update them. *)

type t
(** The abstract type of values representing a player *)

val new_player : string -> t
(** [new_player n] is the player with the name n, cumulative score 0, and an
    emtpy set of words *)

val get_player_name : t -> string
(** [get_player_name p] is the name of the player *)

val get_player_score : t -> int
(** [get_player_score p] is the score of the player *)

val update_score : t -> int -> unit
(** [update_score player] updates the score of the player*)

val get_player_word_list : t -> string list
(** [get_player_wordlist] p is the list of words given to the player *)
