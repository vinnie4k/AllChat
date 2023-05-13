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
