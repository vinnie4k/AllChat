open Stdlib

(****** HELPERS START ******)
let rec get_player_names lst =
  match lst with
  | [] -> []
  | h :: t -> [ Player.get_player_name h ] @ get_player_names t

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

let max_number_list l =
  match l with
  | [] -> failwith "None"
  | h :: t ->
      let rec helper (seen, rest) =
        match rest with
        | [] -> seen
        | h' :: t' ->
            let seen' = if h' > seen then h' else seen in
            let rest' = t' in
            helper (seen', rest')
      in
      helper (h, t)

let rec func x lst c =
  match lst with
  | [] -> raise (Failure "Not Found")
  | hd :: tl -> if hd = x then c else func x tl (c + 1)

let find_list_index x lst = func x lst 0
(* let rec print_list = function | [] -> () | e :: l -> print_int e;
   print_string " "; print_list l *)

(****** HELPERS END ******)

type game_mode =
  | Toxic
  | Wholesome

type game_data = {
  g_mode : game_mode;
  game_file : Get_data.t;
  num_rounds : int;
  num_players : int;
  players : Player.t array;
  scores : int list;
}

(* type running_game = game_data ref *)

let game =
  ref
    {
      g_mode = Wholesome;
      game_file =
        "data/" ^ "test_data.json" |> Yojson.Basic.from_file
        |> Get_data.from_json;
      num_rounds = 0;
      num_players = 0;
      players = Array.make 0 (Player.new_player "");
      scores = [];
    }

let string_of_game () =
  match !game with
  | { g_mode; num_rounds; num_players; players; _ } ->
      "{"
      ^ (match g_mode with
        | Wholesome -> "Wholesome"
        | Toxic -> "Toxic")
      ^ ", " ^ string_of_int num_rounds ^ ", " ^ string_of_int num_players
      ^ ", "
      ^ (get_player_names (Array.to_list players) |> names_separated)
      ^ "}"

let rec make_0_list len lst =
  if List.length lst = len then lst else make_0_list len (0 :: lst)

let get_gf_helper file_name =
  "data/" ^ file_name |> Yojson.Basic.from_file |> Get_data.from_json

let find_game_file g_mode =
  match g_mode with
  | Toxic -> get_gf_helper "toxic_data.json"
  | Wholesome -> get_gf_helper "wholesome_data.json"

let initialize_game g_mode num_p ruds name_array =
  game :=
    {
      g_mode;
      game_file = find_game_file g_mode;
      num_players = num_p;
      num_rounds = ruds;
      players = name_array;
      scores = make_0_list num_p [];
    };
  print_endline (string_of_game ())

(* Array.make num_p 0 |> Array.to_list turns an array into a list while
   initializing*)

let rec listadd a b =
  match a with
  | [] -> b
  | hd :: tl -> (
      match b with
      | hd2 :: tl2 -> (hd + hd2) :: listadd tl tl2
      | [] -> a)

let update_player_scores game_data new_scores =
  let deref_game_data = !game_data in
  let added_list = listadd new_scores deref_game_data.scores in
  let n_game = { !game_data with scores = added_list } in
  game := n_game

(* let update_player_score_extra game_data new_scores = let deref_game_data =
   !game_data in let n_game = { g_mode = deref_game_data.g_mode; num_rounds =
   deref_game_data.num_rounds; num_players = deref_game_data.num_players;
   players = deref_game_data.players; scores = new_scores; } in game_data :=
   n_game *)
let update_rounds game_data num_rounds =
  let n_game = { !game_data with num_rounds } in
  game := n_game

let update_game_mode game_data g_mode =
  let game_file = find_game_file g_mode in
  let n_game = { !game_data with g_mode; game_file } in
  game := n_game

(* let update_game_file game_data g_mode = let game_file = find_game_file g_mode
   in let n_game = { !game_data with game_file } in game := n_game *)

let wrap_up_game game_data =
  let deref_game_data = !game_data in
  for i = 0 to deref_game_data.num_players - 1 do
    Player.update_score
      deref_game_data.players.(i)
      (List.nth deref_game_data.scores i)
  done;
  let n_game =
    {
      deref_game_data with
      scores = List.map (fun _ -> 0) deref_game_data.scores;
    }
  in
  game := n_game

(*Getters*)
let get_did_game_end game_data rnd_num = !game_data.num_rounds <= rnd_num
let get_current_scores game_data = !game_data.scores
let get_game_mode game_data = !game_data.g_mode
let get_gf game_data = !game_data.game_file
let get_num_rounds game_data = !game_data.num_rounds
let get_num_players game_data = !game_data.num_players
let get_players game_data = !game_data.players

let invert_list lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in
  aux [] lst

let tuple_sort lst =
  List.sort
    (fun (k1, v1) (k2, v2) -> if k1 = k2 then compare v1 v2 else compare k1 k2)
    lst
  |> invert_list

let get_winner_helper game_data =
  let deref_game_data = !game_data in
  let score_player_tuple =
    List.combine deref_game_data.scores
      (deref_game_data.players |> Array.to_list)
  in
  tuple_sort score_player_tuple

let rec join_strings = function
  | [] -> ""
  | "" :: t -> join_strings t
  | h :: t -> h ^ " " ^ join_strings t

let get_winner game_data =
  Player.get_player_name
    !game_data.players.(find_list_index
                          (max_number_list !game_data.scores)
                          !game_data.scores)

let get_rankings game_data =
  let score_player_tuple = get_winner_helper game_data in
  let ordered_name_array = Array.make (List.length score_player_tuple) "" in
  for i = 0 to List.length score_player_tuple - 1 do
    Array.set ordered_name_array i
      (Player.get_player_name (Stdlib.snd (List.nth score_player_tuple i)))
  done;
  ordered_name_array |> Array.to_list |> join_strings

let get_cumulative_player_score game_state =
  let deref_game_data = !game_state in
  let player_array = deref_game_data.players in
  let cumulative_score_array = Array.make (Array.length player_array) 0 in
  for i = 0 to Array.length player_array - 1 do
    Array.set cumulative_score_array i
      (Player.get_player_score deref_game_data.players.(i))
  done;
  cumulative_score_array |> Array.to_list
