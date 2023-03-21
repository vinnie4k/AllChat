type input_success = {
  message : string;
  success : bool;
}

(* val ask_forwords : string list -> string -> string list *)
val ask_forwords : string list -> string -> input_success
(** Handling of user inputs. *)
(*Requests user input of words from given word bank*)

val trigger_next_player : int -> int -> unit

val one_player_turn : Player.t -> string list -> string -> bool -> unit
(**Command line output and input handling for a single player taking their turn*)

(* val num_players : int *)
val num_players : int ref
(**The int number of people playing the game*)

val player_list : Player.t array
(** The array consisting of player profiles of type Player.t for every player.
    Array.length player_list = # of players*)

val fill_in_players : unit -> unit

(**GAME START -> fill_in_players -> player turns begin*)
