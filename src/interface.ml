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
  | _ -> false

let rec create_game_mode player_input =
  let s = player_input |> String.trim |> String.lowercase_ascii in
  match s with
  | "wholesome" -> Game_state.Wholesome
  | "w" -> Game_state.Wholesome
  | "toxic" -> Game_state.Toxic
  | "t" -> Game_state.Toxic
  | "shut the fuck up" -> Game_state.Toxic
  | _ -> create_game_mode (invalid_input "game mode")

let rec create_num_players player_input =
  try
    let np = int_of_string player_input in
    if np <= 0 then create_num_players (invalid_input "number of friends")
    else np
  with _ -> create_num_players (invalid_input "number")

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
      (*add correct grammar later*) ^ formatted_word_bank);
     if rnd_num = 1 then
       output_statement
         (* "(type your words in the order they should appear in the sentence, \
            separating the words with spaces)"); *)
         "(type a single word with no spaces)");
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

(* let rec process_response game wrd_strng wrd_lst blanks sent_strng = let
   input_words = words_to_list wrd_strng in if not (List.length input_words =
   blanks) then process_response game (invalid_input "word") wrd_lst blanks
   sent_strng else match word_in_list input_words wrd_lst with | false ->
   process_response game (invalid_input "word") wrd_lst blanks sent_strng | true
   -> (*Get_data.calculate_score file*) [ 1; 2 ] *)

(* let make_score_pairs s_list p_array = if not (List.length s_list =
   Array.length p_array) then raise "players dont all have a score" else let
   pair_list = [] in for ind = 0 to List.length s_list do pair_list = pair_list
   @ () done; pair_list *)

let display_scoreboard game =
  let view_score =
    output_question
      "Type SB to view the current scoreboard or anything else to continue"
  in
  match String.uppercase_ascii view_score with
  | "SB" -> (
      let scores_list = Game_state.get_current_scores game in
      output_statement "Current Leaderboard:";
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

(* let display_end_game_scoreboard game rankings = let scores_list =
   Game_state.get_cumulative_player_score game in output_statement "\nCumulative
   Leaderboard for all games:"; for p = 0 to Game_state.get_num_players game - 1
   do output_statement ("Player " ^ string_of_int (p + 1) ^ ": " ^ rankings.(p)
   ^ " - " ^ string_of_int (List.nth scores_list p) ^ " points") done;
   output_statement "\n" *)

let display_overall_scoreboard game =
  let scores_list = Game_state.get_cumulative_player_score game in
  output_statement "\nCumulative Leaderboard for all games:";
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
