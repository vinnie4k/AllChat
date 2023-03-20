val askforwords : string list -> int -> string list
(** Handling of user inputs. *)
(*Requests user input of words from given word bank*)

type input_success = {
  message : string;
  success : bool;
}

val trigger_next_player : int -> int -> unit

val one_player_turn : Profile.Player.t -> string list -> string -> bool -> unit
(**Command line output and input handling for a single player taking their turn*)
