open Game
open Dungeon

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
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()
