open State
open Cutscene
open Battle
open Keydoor
open Yojson.Basic.Util

type encounter = {
  e_type : string;
  json : string;
  repeatable : bool;
  mutable visited : bool;
}

type dungeon = {
  mutable map : string list;
  mutable encounters : (int * int * encounter) list;
  starting_coords : int * int;
}

let start_coords game = game.starting_coords

let rec encounter_list yoj_list =
  match yoj_list with
  | [] -> []
  | h :: tail ->
      ( to_int (List.assoc "x" (to_assoc h)),
        to_int (List.assoc "y" (to_assoc h)),
        {
          e_type = to_string (List.assoc "type" (to_assoc h));
          json = to_string (List.assoc "json" (to_assoc h));
          repeatable = to_bool (List.assoc "repeatable" (to_assoc h));
          visited = false;
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
        let _ =
          ANSITerminal.print_string [ ANSITerminal.red ] "@";
          print_endline (String.sub h 1 (String.length h - 1))
        in
        print_dungeon player (line + 1)
          {
            map = t;
            encounters = dungeon.encounters;
            starting_coords = dungeon.starting_coords;
          }
      else
        let _ =
          print_string (String.sub h 0 (current_col player - 1));
          ANSITerminal.print_string [ ANSITerminal.red ] "@";
          print_endline
            (String.sub h (current_col player)
               (String.length h - current_col player))
        in
        print_dungeon player (line + 1)
          {
            map = t;
            encounters = dungeon.encounters;
            starting_coords = dungeon.starting_coords;
          }

let update_dungeon_state j_cut player dungeon =
  let json =
    Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ j_cut ^ ".json")
  in
  let exit = to_assoc (List.assoc "end_floor" (to_assoc json)) in
  if to_bool (List.assoc "same" exit) = true then player
  else
    let json' = to_string (List.assoc "name" exit) in
    dungeon.map <-
      Core.In_channel.read_lines ("data" ^ Filename.dir_sep ^ json' ^ ".txt");
    dungeon.encounters <-
      from_json_encounters
        (Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ json' ^ ".json"));
    State.change_coords player
      (to_int (List.assoc "r" exit), to_int (List.assoc "c" exit))

let update_coords (r, c) player = State.change_coords player (r, c)

let cutscene_item file =
  to_string
    (List.assoc "item"
       (to_assoc
          (Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ file ^ ".json"))))

let rec check_encounter player dungeon_encounters (dungeon : dungeon) =
  match dungeon_encounters with
  | [] -> print_endline ""
  | (row, col, enc) :: t ->
      if
        enc.visited = false
        && row = current_row player
        && col = current_col player
      then
        let () = if enc.repeatable = false then enc.visited <- true else () in
        if enc.e_type = "flavor" then print_endline enc.json
        else if enc.e_type = "battle" then (
          Battle.play_game player enc.json;
          enc.visited <- true)
        else if enc.e_type = "cutscene" then (
          let coords = Cutscene.play_game enc.json player in
          if coords = (0, 0) then () else ignore (update_coords coords player);
          if has_item (cutscene_item enc.json) player.inv then
            enc.visited <- true
          else ())
        else if enc.e_type = "keydoor" then
          if Keydoor.play_game enc.json player true then
            (*set true for testing*)
            let _ = update_dungeon_state enc.json player dungeon in
            ()
          else ()
        else failwith "incorrect encounter type"
      else check_encounter player t dungeon

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

let move_player player dr dc line dungeon test =
  if
    check_valid_move
      (current_row player + dr)
      (current_col player + dc)
      0 dungeon
  then (
    let new_player = State.update_coords player dr dc in
    let _ = check_encounter new_player dungeon.encounters dungeon in
    if test then print_dungeon new_player line dungeon)
  else
    let _ = print_endline "Cannot Move There!" in
    if test then print_dungeon player line dungeon

let print_inventory player = print_string (inventory_to_string player)
let print_hp player = print_string (hp_to_string player)

let rec next str player line dungeon =
  let _ = Sys.command "clear" in
  let _ =
    let dr, dc =
      match str with
      | "quit" | "q" ->
          let _ = Sys.command "clear" in
          let _ = print_endline "\n Goodbye! \n" in
          exit 0
      | "w" | "north" -> (-1, 0)
      | "a" | "west" -> (0, -1)
      | "s" | "south" -> (1, 0)
      | "d" | "east" -> (0, 1)
      | "i" | "inventory" ->
          let _ = print_inventory player in
          (0, 0)
      | "hp" ->
          let _ = print_hp player in
          (0, 0)
      | "potion" ->
          let _ = use_potion player (get_inv_lst player) in
          (0, 0)
      | _ ->
          let _ = print_string "**Please enter a valid command**    " in
          (0, 0)
    in
    move_player player dr dc line dungeon true
  in
  next (read_line ()) player line dungeon

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
         'inventory' to access your inventory, and 'potion' to use a potion. \n"
    in
    let _ = print_dungeon player 0 game in
    next (read_line ()) player 0 game
  with Sys_error _ ->
    print_string
      "\nSorry, that file does not appear to exists. Please start again \n"