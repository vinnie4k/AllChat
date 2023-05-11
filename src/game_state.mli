type game_mode =
  | Toxic
  | Wholesome  (** The type representing the the different game modes. *)

type game_data
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
