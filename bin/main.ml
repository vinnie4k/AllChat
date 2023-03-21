open Allchat

(** [start_game f] starts the AllChat game in file [f]. *)
let start_game f =
  Interface.output_statement ("Loading game file " ^ f);
  let num_players =
    int_of_string (Interface.output_question "How many people are playing?")
  in
  let player_list = Array.make num_players (Player.new_player "|*_*|") in
  for i = 0 to num_players - 1 do
    player_list.(i) <- Interface.create_player (i + 1)
  done;
  Interface.output_statement
    ("Welcome to AllChat, "
    ^ (Interface.fetch_player_names (Array.to_list player_list)
      |> Interface.names_separated)
    ^ "!")

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
