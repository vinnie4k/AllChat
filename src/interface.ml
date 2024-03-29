let data_dir_prefix = "data" ^ Filename.dir_sep
let statement_color = ANSITerminal.red
let question_color = ANSITerminal.blue

let output_statement text =
  ANSITerminal.print_string [ statement_color ] ("\n" ^ text ^ "\n")

let output_question text =
  ANSITerminal.print_string [ question_color ] ("\n" ^ text ^ "\n");
  print_string "> ";
  let input = read_line () in
  match input with
  | "#quit" -> failwith "Game Quitted"
  | _ -> input

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

let create_custom_game player_input =
  let s = player_input |> String.trim |> String.lowercase_ascii in
  match s with
  | "y" -> true
  | "yes" -> true
  | "yas" -> true
  | "yep" -> true
  | "custom" -> true
  | "c" -> true
  | _ -> false

let rec create_game_mode player_input =
  let s = player_input |> String.trim |> String.lowercase_ascii in
  match s with
  | "wholesome" -> Game_state.Wholesome
  | "w" -> Game_state.Wholesome
  | "toxic" -> Game_state.Toxic
  | "t" -> Game_state.Toxic
  | "shut up" -> Game_state.Toxic
  | _ -> create_game_mode (invalid_input "game mode")

let rec create_num_players player_input =
  try
    let np = int_of_string player_input in
    if np <= 0 || np > 8 then
      create_num_players (invalid_input "number of players")
    else np
  with _ -> create_num_players (invalid_input "number")

let rec create_num_rounds player_input =
  try
    let np = int_of_string player_input in
    if np <= 0 || np > 8 then
      create_num_rounds (invalid_input "number of rounds")
    else np
  with _ -> create_num_rounds (invalid_input "number")

let rec ask_instructions player_input =
  if
    String.lowercase_ascii player_input <> "yes"
    && String.lowercase_ascii player_input <> "no"
    && String.lowercase_ascii player_input <> "y"
    && String.lowercase_ascii player_input <> "n"
  then ask_instructions (invalid_input "input")
  else
    try
      if String.lowercase_ascii player_input = "yes" then true
      else if String.lowercase_ascii player_input = "y" then true
      else if String.lowercase_ascii player_input = "no" then false
      else if String.lowercase_ascii player_input = "n" then false
      else ask_instructions player_input
    with _ -> ask_instructions (invalid_input "input")

let format_word_bank bank =
  let strin = List.fold_left (fun acc word -> word ^ " | " ^ acc) "" bank in
  String.sub strin 0 (String.length strin - 3)

let get_player n arr = arr.(n)
let words_to_list x = Str.split_delim (Str.regexp " ") x
let word_in_list word bank = List.exists (fun wrd -> wrd = word) bank

let rec check_valid_input repo input bank =
  match input with
  | h :: t ->
      Get_data.includes_word repo h
      && word_in_list h bank
      && check_valid_input repo t bank
  | [] -> true

let rec continue_to_ask repo words_strg bank =
  let wrd_lst = words_to_list words_strg in

  if check_valid_input repo wrd_lst bank then
    match wrd_lst with
    | h :: _ -> h
    | _ ->
        continue_to_ask repo
          (String.lowercase_ascii (invalid_input "response"))
          bank
  else
    continue_to_ask repo
      (String.lowercase_ascii (invalid_input "response"))
      bank

let rec run_round pn data wpr p_array round_sentence rnd_num player_num
    response_list =
  if pn = player_num then
    Get_data.calculate_score data round_sentence response_list
  else
    let bank_list = Get_data.get_word data wpr in
    let formatted_word_bank = format_word_bank bank_list in
    output_statement
      ("\n" ^ Player.get_player_name (get_player pn p_array) ^ "'s turn:");
    output_statement ("\"" ^ round_sentence ^ "\"");
    (let blanks = Get_data.get_blanks data round_sentence in
     output_statement
       ("Fill in " ^ (blanks |> string_of_int) ^ " blanks using:\n"
      ^ formatted_word_bank);
     if rnd_num = 1 then output_statement "(type a single word with no spaces)");
    let response = String.lowercase_ascii (output_question "") in
    let res_wrd_list = words_to_list response in
    if check_valid_input data res_wrd_list bank_list then
      match res_wrd_list with
      | response1 :: _ ->
          run_round (pn + 1) data wpr p_array round_sentence rnd_num player_num
            (response_list @ [ response1 ])
      | _ ->
          let new_response_head = continue_to_ask data response bank_list in
          run_round (pn + 1) data wpr p_array round_sentence rnd_num player_num
            (response_list @ [ new_response_head ])
    else
      let new_response_head = continue_to_ask data response bank_list in
      run_round (pn + 1) data wpr p_array round_sentence rnd_num player_num
        (response_list @ [ new_response_head ])

let display_scoreboard game =
  let view_score =
    output_question
      "Type SB to view the current scoreboard or anything else to continue"
  in
  match String.uppercase_ascii view_score with
  | "SB" -> (
      let scores_list = Game_state.get_current_scores game in
      output_statement "Current Scoreboard:";
      for p = 0 to Game_state.get_num_players game - 1 do
        output_statement
          ("Player "
          ^ string_of_int (p + 1)
          ^ ": "
          ^ Player.get_player_name (get_player p (Game_state.get_players game))
          ^ " - "
          ^ string_of_int (List.nth scores_list p)
          ^ " points")
      done;
      output_statement "\n";
      match output_question "Type anything to continue" with
      | _ -> ())
  | _ -> ()

let display_overall_scoreboard game =
  let scores_list = Game_state.get_cumulative_player_score game in
  output_statement "\nCumulative Scoreboard for all games:";
  for p = 0 to Game_state.get_num_players game - 1 do
    output_statement
      ("Player "
      ^ string_of_int (p + 1)
      ^ ": "
      ^ Player.get_player_name (get_player p (Game_state.get_players game))
      ^ " - "
      ^ string_of_int (List.nth scores_list p)
      ^ " points")
  done;
  output_statement "\n"

let display_overall_ranking rank_name_tuple =
  output_statement "\nCumulative rankings for all games:";
  for p = 0 to List.length rank_name_tuple - 1 do
    output_statement
      (fst (List.nth rank_name_tuple p) ^ " " ^ snd (List.nth rank_name_tuple p))
  done;
  output_statement "\n"

let rec ask_credits player_input =
  let lowercased_input = String.lowercase_ascii player_input in
  if
    lowercased_input <> "yes" && lowercased_input <> "no"
    && lowercased_input <> "y" && lowercased_input <> "n"
  then ask_credits (invalid_input "input (yes or no)")
  else
    try
      if lowercased_input = "yes" then true
      else if lowercased_input = "y" then true
      else if lowercased_input = "no" then false
      else if lowercased_input = "n" then false
      else ask_credits player_input
    with _ -> ask_credits (invalid_input "input (yes or no)")
