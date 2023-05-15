type game_mode =
  | Toxic
  | Wholesome  (** The type representing the the different game modes. *)

type game_data = {
  g_mode : game_mode;
  num_rounds : int;
  num_players : int;
  players : Player.t array;
  scores : int list;
}
(** The abstract data type representing the the game and its state. It includes
    the game mode, score mode, number of players, the player themselves, scores
    for each round for each player, current round, and whether or not the game
    ended. The most important features of game_state is to keep track of the
    players in the game, keep track of rounds, update the player's score based
    on the given score calculated in get_data and interface, as well as keeping
    track of rounds and when the game ends. *)

val game : game_data ref
(** game is a ref that represents the starting game_data *)

val string_of_game : unit -> string
(** [string_of_game] converts game to a string representation *)

val initialize_game : game_mode -> int -> Player.t array -> unit
(** [initialize_game g_mode n lst] initializes initial_game with [n] number of
    players, each with name in [lst], with the game mode [g_mode] *)

val update_player_scores : game_data ref -> int list -> unit
(** [update_player_scores game_data player_score_lst] updates the game data with
    new player scores. *)

val update_game_mode : game_data ref -> game_mode -> unit
(** [update_game_mode game_mode g_mode] updates the game with the game mode
    [g_mode] *)

val wrap_up_game : game_data ref -> unit
(** [wrap_up_game game_data] updates the game data once it ends. It inputs the
    scores of the players this game into their permanent record in the player.ml
    file. It also sets all points this game to 0 so the game can be reused, this
    is the same function as initialize_game, but we do it twice for safety
    reasons. *)

val get_did_game_end : game_data ref -> int -> bool
(** [did_game_end game_data rnd_num] game is whether or not the game finished. *)

val get_current_scores : game_data ref -> int list
(** [get_score_total_list game_data] turns the array of total scores into a list
    of total scores. *)

val get_game_mode : game_data ref -> game_mode
(** [get_game_mode game_data] returns the current game_mode of the game. *)

val get_num_rounds : game_data ref -> int
(** [get_num_rounds game_data] returns the total length of the game in rounds. *)

val get_num_players : game_data ref -> int
(** [get_num_players game_data] returns the total number of players. *)

val get_players : game_data ref -> Player.t array
(** [get_players game_data] returns the array of players, where array index 0
    corresponds to player 1, array index 2 corresponds to player 2, etc... *)

val get_winner : game_data ref -> string
(** [get_winner game_data] returns the winner of the game by their score *)

val get_rankings : game_data ref -> string
(** [get_rankings game_data] returns the ranking of the players in descending
    order by their points. For example, if Liam has 100 points, and Charlie has
    50, then Liam is ranked before Charlie because 100>50 *)

val get_cumulative_player_score : game_data ref -> int list
(** [get_cumulative_player_score game_data] returns the list of all players's
    scores *)
