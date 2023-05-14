type t = {
  player_name : string;
  mutable player_score : int;
  player_word_list : string list;
}

let new_player n = { player_name = n; player_score = 0; player_word_list = [] }

let get_player_name p = p.player_name

let get_player_score p = p.player_score

let get_player_word_list p = p.player_word_list

let update_score player p = 
  let updated_score = player.player_score + p in
  player.player_score <- updated_score

