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

  (* for pn = 0 to player_num player number do *)
  let responses =
    Interface.run_round 0 data wpr p_array round_sentence rnd_num player_num []
  in
  Game_state.update_player_scores Game_state.game responses

(* Interface.process_response response "" *)

(** [start_game f] starts the AllChat game in file [f]. *)
let rec consecutive_games game_data =
  Interface.output_statement
    "Thank you for choosing to play again! Happy round two :)";
  let game_mode =
    Interface.create_game_mode
      (Interface.output_question "Enter the game mode (Toxic or Wholesome): ")
  in
  Interface.output_statement
    ("Welcome back to the game! "
    ^ (Interface.fetch_player_names
         (Array.to_list (Game_state.get_players game_data))
      |> Interface.names_separated)
    ^ "!");
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
  Game_state.wrap_up_game Game_state.game;
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
  (* let custom = Interface.create_custom_game (Interface.output_question "Would
     you like to customize your game rules?") in *)
  let game_mode =
    Interface.create_game_mode
      (Interface.output_question "Enter the game mode (Toxic or Wholesome): ")
  in
  let num_players =
    Interface.create_num_players
      (Interface.output_question "How many people are playing?")
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
  Game_state.initialize_game game_mode num_players player_list;
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
    ("\nThe ranking for this game is "
    ^ Game_state.get_winner Game_state.game
    ^ "!" ^ "\nHere are the rankings in order:\n"
    ^ Game_state.get_rankings Game_state.game);
  Game_state.wrap_up_game Game_state.game;
  Interface.display_overall_scoreboard Game_state.game;
  let output =
    Interface.output_question
      "To start a new game, type NEW, otherwise, type END or anything else"
  in
  match String.uppercase_ascii output with
  | "NEW" -> consecutive_games Game_state.game
  | "END" -> ()
  | _ -> ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  Interface.output_statement
    "\nWelcome to AllChat, the game where we rate your toxicity ☠️.";
  let output =
    Interface.output_question
      "Please enter the name of the game file you want to load:"
  in
  match output with
  | exception End_of_file -> ()
  | file_name -> start_game (Interface.data_dir_prefix ^ file_name ^ ".json")

(* Execute the game engine. *)
let () = main ()
