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

val invalid_input : string -> string
(** [invalid_input x] returns a string message for an invalid [x] *)

val create_game_mode : string -> Game_state.game_mode
(** [create_game_mode x] is Some Game_state.game_mode if [x] is a valid game
    mode or None if not *)

val get_player : int -> Player.t array -> Player.t
(** [get_player n arr] is the player at index [n] in [arr] *)

val format_word_bank : string list -> string
(** [format_word_bank bank] formats a string list of words [bank] into an
    outputtable format *)

val words_to_list : string -> string list
(** [words_to_list x] converts [x] to a list of strings, separated by a space *)

val process_response : string -> string -> unit
(** [process_response x sentence] processes the user input [x] and [sentence]
    and checks if the number of words is valid *)

val display_scoreboard : Game_state.game_data ref -> unit
(** [display_scoreoard game] displays a formatted scoreboard of each player's 
    scores in the game [game]*)