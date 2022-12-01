open State
open Cutscene
open Yojson.Basic.Util

type encounter = {
  e_type : string;
  json : string;
}
(**RI: [e_type] is either "flavor", "battle", or "cutscene"*)

type dungeon = {
  map : string list;
  encounters : (int * int * encounter) list;
  starting_coords : int * int;
}

let start_coords game = game.starting_coords

(**[print_dungeon player line dungeon] prints the dungeon with the player placed
   at the coordinates given by [player]*)
let rec print_dungeon player line dungeon =
  match dungeon.map with
  | [] -> ()
  | h :: t ->
      if line != current_row player then
        let _ = print_endline h in
        print_dungeon player (line + 1)
          {
            map = t;
            encounters = dungeon.encounters;
            starting_coords = dungeon.starting_coords;
          }
      else if line = current_row player && current_col player = 0 then
        let _ = print_endline ("@" ^ String.sub h 1 (String.length h - 1)) in
        print_dungeon player (line + 1)
          {
            map = t;
            encounters = dungeon.encounters;
            starting_coords = dungeon.starting_coords;
          }
      else
        let _ =
          print_endline
            (String.sub h 0 (current_col player - 1)
            ^ "@"
            ^ String.sub h (current_col player)
                (String.length h - current_col player))
        in
        print_dungeon player (line + 1)
          {
            map = t;
            encounters = dungeon.encounters;
            starting_coords = dungeon.starting_coords;
          }

let rec check_encounter player dungeon_encounters =
  match dungeon_encounters with
  | [] -> print_endline ""
  | (row, col, enc) :: t ->
      if row = current_row player && col = current_col player then (
        if enc.e_type = "flavor" then print_endline enc.json
        else if enc.e_type = "cutscene" then Cutscene.play_game enc.json
        else if enc.e_type = "battle" then failwith "unimplemented"
          (*Battle.play_game enc.json*))
      else check_encounter player t

(** [check_valid_move r c line dungeon] checks if the [r] [c] are a valid place
    to move in the dungeon (a period '.' or a hashtag '#') *)
let rec check_valid_move r c line dungeon =
  match dungeon.map with
  | [] -> false
  | h :: t ->
      if r = line then
        if h.[c - 1] = '.' || h.[c - 1] = '#' then true else false
      else
        check_valid_move r c (line + 1)
          {
            map = t;
            encounters = dungeon.encounters;
            starting_coords = dungeon.starting_coords;
          }

(*[move_player player dr dc line dungeon] updates [player]'s coords and prints
  out new dungeon with updated coordinates*)
let move_player player dr dc line dungeon =
  if
    check_valid_move
      (current_row player + dr)
      (current_col player + dc)
      0 dungeon
  then
    let new_player = State.update_coords player dr dc in
    let _ = check_encounter new_player dungeon.encounters in
    print_dungeon new_player line dungeon
  else
    let _ = print_endline "Cannot Move There!" in
    print_dungeon player line dungeon

(**[print_inventory player] prints the inventory of the player into the terminal*)
let print_inventory player = print_endline (inventory_to_string player)

(**[next str player line dungeon] recursively checks for player input and either
   quits or calls [move_player] according*)
let rec next str player line dungeon =
  let _ = Sys.command "clear" in
  let _ =
    let dr, dc =
      match str with
      | "quit" | "q" ->
          let _ = print_endline "\n Goodbye! \n" in
          exit 0
      | "w" | "north" -> (-1, 0)
      | "a" | "west" -> (0, -1)
      | "s" | "south" -> (1, 0)
      | "d" | "east" -> (0, 1)
      | "i" | "inventory" ->
          let _ = print_inventory player in
          (0, 0)
      | _ ->
          let _ = print_string "**Please enter a valid command**" in
          (0, 0)
    in
    move_player player dr dc line dungeon
  in
  next (read_line ()) player line dungeon

let rec encounter_list yoj_list =
  match yoj_list with
  | [] -> []
  | h :: tail ->
      ( to_int (List.assoc "x" (to_assoc h)),
        to_int (List.assoc "y" (to_assoc h)),
        {
          e_type = to_string (List.assoc "type" (to_assoc h));
          json = to_string (List.assoc "json" (to_assoc h));
        } )
      :: encounter_list tail

let from_json_encounters json =
  encounter_list (to_list (List.assoc "encounters" (to_assoc json)))

let from_json_start json =
  let coords = to_list (List.assoc "starting coordinates" (to_assoc json)) in
  match coords with
  | x :: y :: _ ->
      ( to_int (List.assoc "x" (to_assoc x)),
        to_int (List.assoc "y" (to_assoc y)) )
  | _ -> failwith "no starting coordinates in json"

(**[from_txt str] returns a record that includes string list where each line of
   given txt file is a separate string in the list, a list of encounters from a
   json file, and the starting coords of the dungeon *)
let from_txt text =
  {
    map = Core.In_channel.read_lines ("data" ^ Filename.dir_sep ^ text ^ ".txt");
    encounters =
      from_json_encounters
        (Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ text ^ ".json"));
    starting_coords =
      from_json_start
        (Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ text ^ ".json"));
  }

let play_game text =
  try
    let game = from_txt text in
    let player =
      State.init_state (fst (start_coords game)) (snd (start_coords game))
    in
    let _ = Sys.command "clear" in
    let _ =
      print_string
        "Use 'wasd' + enter to move. Type 'quit' to quit; Use 'i' or \
         'inventory' to access your inventory \n"
    in
    let _ = print_dungeon player 0 game in
    next (read_line ()) player 0 game
  with Sys_error _ ->
    print_string
      "\nSorry, that file does not appear to exists. Please start again \n"