open Game
open Dungeon
open State

let player = State.init_state 1 5

(**[print_dungeon player line dungeon] prints the dungeon along with the current
   [player]s coordinates*)
let rec print_dungeon player line = function
  | [] -> ()
  | h :: t ->
      if line != current_row player then
        let _ = print_endline h in
        print_dungeon player (line + 1) t
      else if line = current_row player && current_col player = 0 then
        let _ = print_endline ("@" ^ String.sub h 1 (String.length h - 1)) in
        print_dungeon player (line + 1) t
      else
        let _ =
          print_endline
            (String.sub h 0 (current_col player - 1)
            ^ "@"
            ^ String.sub h (current_col player)
                (String.length h - current_col player))
        in
        print_dungeon player (line + 1) t

(*[check_valid_move r c line dungeon] checks if the r c are a valid place to
  move in the dungeon (a period '.' or a hashtag '#') *)
let rec check_valid_move r c line = function
  | [] -> false
  | h :: t ->
      if r = line then
        if h.[c - 1] = '.' || h.[c - 1] = '#' then true else false
      else check_valid_move r c (line + 1) t

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
    print_dungeon new_player line dungeon
  else print_endline "\n Cannot Move There! \n"

(**[print_inventory player] prints the inventory of the player into the terminal*)
let print_inventory player = print_endline (inventory_to_string player)

(**[next str player line dungeon] recursively checks for player input and either
   quits or calls [move_player] according*)
let rec next str player line dungeon =
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
          let _ = print_endline "\n **Please enter a valid command** \n" in
          (0, 0)
    in
    move_player player dr dc line dungeon
  in
  next (read_line ()) player line dungeon

(** [play_game text] starts the dungeon in file [text]. *)
let play_game text =
  try
    let game = from_txt text in
    let _ =
      print_endline
        "\n\
        \ Use 'wasd' + enter to move. Type 'quit' to quit \n\
        \ Use 'i' or 'inventory' to access your inventory \n"
    in
    let _ = print_dungeon player 0 game in
    next (read_line ()) player 0 game
  with Sys_error _ ->
    print_string
      "\nSorry, that file does not appear to exists. Please start again \n"

(** [main ()] presents the start screen and level selector. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ] "\n\nDUNGEON QUEST\n";
  print_endline
    "Your adventure into the dungeons will soon begin. \n\
    \ \n\
    \ Please enter the name of the level you wish to load: \n\
    \ - dungeon \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game ("data" ^ Filename.dir_sep ^ file_name ^ ".txt")

(* Execute the game engine. *)
let () = main ()
