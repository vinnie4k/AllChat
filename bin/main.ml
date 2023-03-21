let data_dir_prefix = "data" ^ Filename.dir_sep

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Allchat, the game where we rate your toxicity ☠️.\n";
  print_endline "Please enter the name of the game file you want to load:\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> print_endline (data_dir_prefix ^ file_name ^ ".json")

(* let *)

(* match read_line () with *)
(* | exception End_of_file -> () *)
(* | file_name -> play_game (data_dir_prefix ^ file_name ^ ".json") *)

(* Execute the game engine. *)
let () = main ()
