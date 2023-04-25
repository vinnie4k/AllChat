type t
(** The abstract data type representing the the game and its state. *)

val get_round_score : t -> int
(** [get_round_score] game is the score gained or lost for every player in the current round. *)

val get_round_score_total : t -> int
(** [get_round_score_total] game is the cumulative score for every player in the current round. *)

val get_round : t -> int
(** [get_round] game is the round that the game is currently on. *)

val did_game_end : t -> bool
(** [did_game_end] game is whether or not the game finished. *)

val initialize_game : int -> string list -> string -> string -> t
(** [initialize_game] n name_lst game_mode score_mode initializes the game with number of players n, each with name in name_lst, with game mode being game_mode and score mode being score_mode. *)

val get_winner : t -> string
(** [get_winner] game is the winner of the round, call this function if game ends to determine final winner. *)