(****** HELPERS START ******)
let rec get_player_names lst =
  match lst with
  | [] -> []
  | h :: t -> [ Player.get_player_name h ] @ get_player_names t

let rec names_separated lst =
  if List.length lst = 1 then
    match lst with
    | [] -> ""
    | h :: _ -> h
  else if List.length lst <= 2 then
    match lst with
    | [] -> ""
    | h :: t -> h ^ " and " ^ names_separated t
  else
    match lst with
    | [] -> ""
    | h :: t -> h ^ ", " ^ names_separated t

(****** HELPERS END ******)

type game_mode =
  | Toxic
  | Wholesome

type game_data = {
  g_mode : game_mode;
  num_rounds : int;
  num_players : int;
  players : Player.t array;
}

(* type running_game = game_data ref *)

let game =
  ref
    {
      g_mode = Wholesome;
      num_rounds = 0;
      num_players = 0;
      players = Array.make 0 (Player.new_player "");
    }

let string_of_game () =
  match !game with
  | { g_mode; num_rounds; num_players; players } ->
      "{"
      ^ (match g_mode with
        | Wholesome -> "Wholesome"
        | Toxic -> "Toxic")
      ^ ", " ^ string_of_int num_rounds ^ ", " ^ string_of_int num_players
      ^ ", "
      ^ (get_player_names (Array.to_list players) |> names_separated)

let initialize_game g_mode num_p name_array =
  game := { g_mode; num_rounds = 3; num_players = num_p; players = name_array };
  print_endline (string_of_game ())

(*Getters*)
let get_did_game_end game_data rnd_num = (!game_data.num_rounds) >= rnd_num
  
let get_score_total game_data = Array.map Player.get_player_score !game_data.players

let get_score_total_list game_data = Array.map Player.get_player_score !game_data.players |> Array.to_list

(*[max_score_finder] lst is a helper function to find the max score in the round score array*) 
let rec max_score_finder lst = match lst with | [] -> 0 |
h :: t -> if h > max_score_finder t then h else max_score_finder t

(*[find_max_score_index] round_score_total max_score index is a helper function to match the max score with the actual index*) 
let rec find_max_score_index round_score_total max_score index = if
round_score_total.(index) = max_score then index else find_max_score_index
round_score_total max_score (index + 1)

let get_winner game_data = Array.get !game_data.players
(find_max_score_index (get_score_total game_data) (max_score_finder
(get_score_total_list game_data)) 0) |> Player.get_player_name