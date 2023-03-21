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
