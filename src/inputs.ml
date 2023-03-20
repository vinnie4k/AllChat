open Profile

(*checks if words inputted are actually from the word bank*)
let rec valid_words request bank =
  match request with
  | [] -> true
  | h :: t -> List.exists (fun s -> s = h) bank && valid_words t bank

let ugly_list_of_string s = String.split_on_char ' ' s
let nice_list_of_string sl = List.filter (fun s -> s <> "") sl
let parse str = str |> ugly_list_of_string |> nice_list_of_string

let rec strlst_to_str = function
  | [] -> ""
  | h :: t -> h ^ " | " ^ strlst_to_str t

type input_success = {
  message : string;
  success : bool;
}

let askforwords (bank : string list) (sentence : string) =
  (*print instructions*)
  let input = parse (read_line ()) in
  if valid_words input bank then
    { message = Get_data.add_words sentence input; success = true }
  else
    {
      message = "Please input the appropriate number of words from the bank";
      success = false;
    }

let rec try_again_turn bank sentence =
  print_endline "> ";
  let succ = askforwords bank sentence in
  print_endline succ.message;
  if succ.success then get_next_player else try_again_turn bank sentence

let rec one_player_turn player bank sentence next =
  print_endline (Player.get_player_name player ^ "'s turn:\n");
  print_endline (sentence ^ "\n");
  print_endline "Fill in " ^ Get_data.get_blanks sentence ^ "blanks using:\n";
  print_endline (strlst_to_str bank);
  print_string "> ";
  let succ = askforwords bank sentence in
  print_endline succ.message;
  if succ.success then get_next_player else try_again_turn bank sentence
