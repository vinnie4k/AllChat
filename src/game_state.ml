open Player

type game_mode = Toxic | Wholesome

type score_mode = Autoscore | Playscore

type t = {game_mode : game_mode; score_mode : score_mode; game_length: int; num_of_player : int; players : Player.t array; round_scores : int array; current_round : int; game_end : bool}

let initialize_game game_mode score_mode game_length num_of_players name_lst = 
  {game_mode = game_mode; score_mode = score_mode; game_length = game_length; num_of_player = num_of_players; players = List.map Player.new_player name_lst |> Array.of_list; round_scores = Array.make num_of_players 0; current_round = 1; game_end = false}


(*retrieval/access functions*)
let get_round_score game_state = game_state.round_scores
(*for devs, the way we determine who has what score is by their index in the array. for example, if Liam is player 1, then his index is 0 in the array. His score corresponds to index 0 of the score array*)

let get_round_score_total game_state = Array.map Player.get_player_score game_state.players

let get_round game_state = game_state.current_round

let did_game_end game_state= game_state.game_end

let get_winner game_state = Array.get game_state.players 0 |> Player.get_player_name


(*modifying functions*)