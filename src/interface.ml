let data_dir_prefix = "data" ^ Filename.dir_sep
let statement_color = ANSITerminal.red
let question_color = ANSITerminal.blue

let output_statement text =
  ANSITerminal.print_string [ statement_color ] ("\n" ^ text ^ "\n")

let output_question text =
  ANSITerminal.print_string [ question_color ] ("\n" ^ text ^ "\n");
  print_string "> ";
  read_line ()

let create_player p =
  Player.new_player
    (output_question ("Enter the name for Player " ^ string_of_int p))

let rec fetch_player_names lst =
  match lst with
  | [] -> []
  | h :: t -> [ Player.get_player_name h ] @ fetch_player_names t

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

let invalid_input ans =
  let outpt =
    output_question
      ("That is not a valid " ^ ans ^ ". Please enter a valid " ^ ans ^ ".")
  in
  outpt

let rec create_game_mode player_input =
  let s = player_input |> String.trim |> String.lowercase_ascii in
  match s with
  | "wholesome" -> Game_state.Wholesome
  | "toxic" -> Game_state.Toxic
  | _ -> create_game_mode (invalid_input "game mode")

let format_word_bank bank =
  let strin = List.fold_left (fun acc word -> word ^ " | " ^ acc) "" bank in
  String.sub strin 0 (String.length strin - 3)

let get_player n arr = arr.(n)
let words_to_list x = Str.split_delim (Str.regexp " ") x
let process_response wrd_strng sent_strng = print_string (wrd_strng ^ sent_strng)

let display_scoreboard game = let view_score = Interface.output_question
    "Type SB to view the current scoreboard or anything else to continue"
    in match (String.uppercase_ascii view_score) with 
    | "SB" -> let scores_list = Game_state.get_score_total_list game in 
    Interface.output_statement ("Current Leaderboard:");
    for p = 0 to Game_state.get_num_players game do
      Interface.output_statement ("Player "^string_of_int(p+1)^": "^
      (Player.get_player_name (Interface.get_player p (Game_state.get_players game)))^
      " - " ^ string_of_int (List.nth scores_list p)^ " points");
    done;
    Interface.output_statement ("\n");
    (match Interface.output_question
    "Type anything to continue" with |_ ->() )
    | _ -> ()
