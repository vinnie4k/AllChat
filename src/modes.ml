type game_mode = Toxic | Wholesome

type score_mode = Autoscore | Playscore

type t = { game_mode : game_mode; score_mode : score_mode; game_length: int }

let get_game_mode a = a.game_mode

let get_score a = a.score_mode

let get_length a = a.game_length