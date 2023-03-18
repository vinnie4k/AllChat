module type Player = sig
  type t
  (** The abstract type of values representing a player*)

  val get_player_name : t -> string
  (** [get_player_name] p is the name of the player*)

  val get_player_score : t -> int
  (** [get_player_score] p is the score of the player*)

  val get_player_wordlist : t -> string list
  (** [get_player_wordlist] p is the list of words given to the player*)

end