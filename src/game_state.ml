open Player

type game_mode = Toxic | Wholesome

type score_mode = Autoscore | Playscore

type t = {game_mode : game_mode; score_mode : score_mode; game_length: int; num_of_player : int; players : Player.t array; round_scores : int array; mutable current_round : int}

let initialize_game game_mode score_mode game_length num_of_players name_lst = 
  {game_mode = game_mode; score_mode = score_mode; game_length = game_length; num_of_player = num_of_players; players = List.map Player.new_player name_lst |> Array.of_list; round_scores = Array.make num_of_players 0; current_round = 1}


(*retrieval/access functions*)
let get_round_score game_state = game_state.round_scores
(*for devs, the way we determine who has what score is by their index in the array. for example, if Liam is player 1, then his index is 0 in the array. His score corresponds to index 0 of the score array*)

let get_round_score_total game_state = Array.map Player.get_player_score game_state.players

let get_round game_state = game_state.current_round

let did_game_end game_state= game_state.current_round >= game_state.game_length

(*[get_round_score_total_list] game_state is a helper function to turn the array of total scores into a list of total scores*)
let get_round_score_total_list game_state = Array.map Player.get_player_score game_state.players |> Array.to_list

(*[max_score_finder] lst is a helper function to find the max score in the round score array*)
let rec max_score_finder lst=
  match lst with
  | [] -> 0
  | h :: t -> if h > max_score_finder t then h else max_score_finder t

(*[find_max_score_index] round_score_total max_score index is a helper function to match the max score with the actual index*)
let rec find_max_score_index round_score_total max_score index =
if round_score_total.(index) = max_score then index
else find_max_score_index round_score_total max_score (index+1)

let get_winner game_state = Array.get game_state.players (find_max_score_index (get_round_score_total game_state) (max_score_finder (get_round_score_total_list game_state)) (0)) |> Player.get_player_name

(*modifying functions*)
let update_player_score game player_index score = Player.update_score (Array.get game.players player_index) score

let update_round game = game.current_round <- (game.current_round + 1)