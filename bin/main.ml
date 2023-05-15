open Allchat

let file_name = "test_data.json"

(* [gf] is the game file stored as a words repo of type Get_data.t *)
let gf = "data/" ^ file_name |> Yojson.Basic.from_file |> Get_data.from_json

(* [wpr] is a constant representing the number of words a player gets per
   round *)
let wpr = 6

let play_round data rnd_num player_num p_array =
  Interface.output_statement ("\nROUND\n   " ^ string_of_int rnd_num ^ " BEGIN!");
  let round_sentence = Get_data.get_sentence data in
  let responses =
    Interface.run_round 0 data wpr p_array round_sentence rnd_num player_num []
  in
  Game_state.update_player_scores Game_state.game responses

(** [start_game f] starts the AllChat game in file [f]. *)
let rec consecutive_games game_data =
  let to_view_credits =
    Interface.output_statement
      "Thank you for choosing to play again! Happy round two :)";
    Interface.ask_credits
      (Interface.output_question
         "Before you continue, would you like to view the names of the \
          beautiful people that created this game? (Yes or No)")
  in
  if to_view_credits then (
    Interface.output_statement
      "This game is created by Vin Bui (vdb23), Charlie Wright (caw253), Liam \
       Du (ld386), and Enjie Wang (ew438) \n\
       for our Spring 2023 CS 3110: Data Structures and Functional Programming \
       final project.\n\n\
       Again, thank you for taking the time to try out our game :)";
    consecutive_games Game_state.game)
  else
    let game_mode =
      Interface.create_game_mode
        (Interface.output_question "Enter the game mode (Toxic or Wholesome): ")
    in
    let num_rounds =
      Interface.create_num_rounds
        (Interface.output_question
           "How many rounds do you want to play? (1 - 8)")
    in
    Interface.output_statement
      ("Welcome back to the game! "
      ^ (Interface.fetch_player_names
           (Array.to_list (Game_state.get_players game_data))
        |> Interface.names_separated)
      ^ "!");
    Game_state.update_rounds game_data num_rounds;
    Game_state.update_game_mode game_data game_mode;
    (*playing rounds*)
    for rnd = 1 to Game_state.get_num_rounds Game_state.game do
      play_round gf rnd
        (Game_state.get_num_players Game_state.game)
        (Game_state.get_players Game_state.game);
      Interface.output_statement ("\nRound " ^ string_of_int rnd ^ " complete!");
      Interface.display_scoreboard Game_state.game
    done;
    Interface.output_statement
      ("\nThe winner of this game is "
      ^ Game_state.get_winner Game_state.game
      ^ "!");
    Interface.output_statement
      ("\nHere are the rankings in order:\n"
      ^ Game_state.get_rankings Game_state.game);
    Game_state.wrap_up_game Game_state.game;
    Interface.display_overall_ranking
      (Game_state.get_cumulative_rankings Game_state.game);
    Interface.display_overall_scoreboard Game_state.game;
    let output =
      Interface.output_question
        "To start a new game, type NEW, otherwise, type END or anything else"
    in
    match String.uppercase_ascii output with
    | "NEW" -> consecutive_games Game_state.game
    | "END" -> ()
    | _ -> ()

let start_game f =
  Interface.output_statement ("Loading game file " ^ f);
  let game_mode =
    Interface.create_game_mode
      (Interface.output_question "Enter the game mode (Toxic or Wholesome): ")
  in
  let num_players =
    Interface.create_num_players
      (Interface.output_question "How many people are playing? (1 - 8)")
  in
  let num_rounds =
    Interface.create_num_rounds
      (Interface.output_question "How many rounds do you want to play? (1 - 8)")
  in
  let player_list = Array.make num_players (Player.new_player "|*_*|") in
  for i = 0 to num_players - 1 do
    player_list.(i) <- Interface.create_player (i + 1)
  done;
  Interface.output_statement
    ("Welcome to AllChat, "
    ^ (Interface.fetch_player_names (Array.to_list player_list)
      |> Interface.names_separated)
    ^ "!");
  Game_state.initialize_game game_mode num_players num_rounds player_list;
  Interface.output_statement
    ("\nHey, "
    ^ (Interface.fetch_player_names (Array.to_list player_list)
      |> Interface.names_separated)
    ^ "!" ^ "\n " ^ "\nHere is a quick guide to this game:"
    ^ "\n\
       1. At any point of the game, if you want to quit the game, just type \
       #quit."
    ^ "\n\
       2. This game intelligently grades your responses against other players, \
       so please try your best!."
    ^ "\n3. Lastly, don't take it too seriously and have fun :).");

  (*playing rounds*)
  for rnd = 1 to Game_state.get_num_rounds Game_state.game do
    play_round
      (Game_state.get_gf Game_state.game)
      rnd
      (Game_state.get_num_players Game_state.game)
      (Game_state.get_players Game_state.game);
    Interface.output_statement ("\nRound " ^ string_of_int rnd ^ " complete!");
    Interface.display_scoreboard Game_state.game
  done;
  Interface.output_statement
    ("\nThe winner of this game is "
    ^ Game_state.get_winner Game_state.game
    ^ "!");
  Interface.output_statement
    ("\nHere are the rankings in order for only this game:\n"
    ^ Game_state.get_rankings Game_state.game);
  Game_state.wrap_up_game Game_state.game;
  Interface.display_overall_ranking
    (Game_state.get_cumulative_rankings Game_state.game);
  Interface.display_overall_scoreboard Game_state.game;
  let output =
    Interface.output_question
      "To start a new game, type NEW, otherwise, type END or anything else"
  in
  match String.uppercase_ascii output with
  | "NEW" -> consecutive_games Game_state.game
  | "END" -> ()
  | _ -> ()

let rec load_game_file output =
  match output with
  | exception End_of_file -> ()
  | file_name ->
      if Sys.file_exists (Interface.data_dir_prefix ^ file_name ^ ".json") then
        start_game (Interface.data_dir_prefix ^ file_name ^ ".json")
      else (
        Interface.output_statement
          ("The provided game file " ^ output
         ^ " does not exist. Please try again or type default to use default \
            game files.");
        let output =
          Interface.output_question
            "Please enter the name of the game file you want to load:"
        in
        match output with
        | exception End_of_file -> ()
        | "default" -> load_game_file "wholesome_data"
        | "Default" -> load_game_file "wholesome_data"
        | "DEFAULT" -> load_game_file "wholesome_data"
        | "D" -> load_game_file "wholesome_data"
        | "d" -> load_game_file "wholesome_data"
        | file_name -> load_game_file file_name)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  Interface.output_statement
    "\nWelcome to AllChat, the game where we rate your toxicity ☠️.";
  let instructions =
    Interface.ask_instructions
      (Interface.output_question
         "Would you like to view the instructions? (Yes or No)")
  in
  if instructions then (
    Interface.output_statement
      "There are two different game modes to choose: (1) Toxic or (2) \
       Wholesome.\n\n\
       The Toxic gamemode will have more negative words and will favor \
       negativity \n\
       whereas the Wholesome gamemode will have more positive words and will \
       favor positivity.\n\n\
       You can play this game with up to 8 players.\n\
       Each player will be given a sentence with a word bank in which they \
       will fill the sentence with the word of their choice.\n\
       Once everybody has had a chance to enter their word into the sentence, \
       the players can view their scores.\n\n\
       There are different game files that you can load that are provided for \
       you, or you can even write your own!\n\
       You can quit anytime by typing '#quit' or by pressing CMD + C (or CTRL \
       + C) on your keyboard to exit out of the terminal.\n\n\n\
       May the most toxic ☠️ (or wholesome ❤️ ) person win!";
    let request_custom =
      Interface.output_question
        "Type \"custom\" to begin loading a custom game file. Otherwise, type \
         anything else."
    in
    let output =
      match Interface.create_custom_game request_custom with
      | false -> "wholesome_data"
      | true ->
          Interface.output_question
            "Please enter the name of the game file you want to load:"
    in
    (* else let output = *)
    match output with
    | exception End_of_file -> ()
    | file_name -> load_game_file file_name)
  else
    let request_custom =
      Interface.output_question
        "Type \"custom\" to begin loading a custom game file. Otherwise, type \
         anything else."
    in
    let output =
      match Interface.create_custom_game request_custom with
      | false -> "wholesome_data"
      | true ->
          Interface.output_question
            "Please enter the name of the game file you want to load:"
    in
    match output with
    | exception End_of_file -> ()
    | file_name -> load_game_file file_name

(* Execute the game engine. *)
let () = main ()
