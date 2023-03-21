val data_dir_prefix : string
(** [data_dir_prefix] is the data directory prefix *)

val question_color : ANSITerminal.style
(** [question_color] is the ANSITerminal style color [c] for displaying
    questions *)

val statement_color : ANSITerminal.style
(** [statement_color] is the ANSITerminal style color [c] for displaying
    questions *)

val output_statement : string -> unit
(** [output_statement text] prints out [text] with color
    constants.statement_color and a new line before and after *)

val output_question : string -> string
(** [output_question text] prints out [text] with color constants.question_color
    and a new line before and after followed by > *)

val create_player : int -> Player.t
(** [create_player p] creates a Player.t for player number [p] *)

val fetch_player_names : Player.t list -> string list
(** [fetch_player_names arr] is a list of player names in [arr] *)

val names_separated : string list -> string
(** [names_separated lst] is a string containing the names in [lst] separated by
    a comma. If there are only two players, then the strings are separated by
    'and'. *)
