type t = {player_name : string; player_score : int; player_word_list : string list}

let new_player n = {player_name = n; player_score = 0; player_word_list = []}

let get_player_name p = p.player_name

let get_player_score p = p.player_score

let get_player_word_list p = p.player_word_list