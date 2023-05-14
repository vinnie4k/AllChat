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

(* let rec word_in_list input bank = match input with | h :: t -> List.exists
   (fun wrd -> wrd = h) bank || word_in_list t bank | [] -> false *)

let rec run_round pn data wpr p_array round_sentence rnd_num player_num
    response_list =
  if pn = player_num then
    Get_data.calculate_score data round_sentence response_list
  else
    let formatted_word_bank = format_word_bank (Get_data.get_word data wpr) in
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
    let response = output_question "" in
    run_round (pn + 1) data wpr p_array round_sentence rnd_num player_num
      (response_list @ [ response ])

(* let rec process_response game wrd_strng wrd_lst blanks sent_strng = let
   input_words = words_to_list wrd_strng in if not (List.length input_words =
   blanks) then process_response game (invalid_input "word") wrd_lst blanks
   sent_strng else match word_in_list input_words wrd_lst with | false ->
   process_response game (invalid_input "word") wrd_lst blanks sent_strng | true
   -> (*Get_data.calculate_score file*) 1::2::[] *)

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
