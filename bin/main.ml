open Game
open Dungeon

(** [main ()] presents the start screen and level selector. *)
let main () =
  let titlestring =
    "   ___    __ __  ____    ____    ___   ___   ____        ___   __ __    \
     ___  _____ ______ \n\
    \  |   \\  |  |  ||    \\  /    |  /  _] /   \\ |    \\      /   \\ |  |  \
     |  /  _]/ ___/|      |\n\
    \  |    \\ |  |  ||  _  ||   __| /  [_ |     ||  _  |    |     ||  |  | /  \
     [_(   \\_ |      |\n\
    \  |  D  ||  |  ||  |  ||  |  ||    _]|  O  ||  |  |    |  Q  ||  |  ||    \
     _]\\__  ||_|  |_|\n\
    \  |     ||  :  ||  |  ||  |_ ||   [_ |     ||  |  |    |     ||  :  ||   \
     [_ /  \\ |  |  |  \n\
    \  |     ||     ||  |  ||     ||     ||     ||  |  |    |     ||     \
     ||     |\\    |  |  |  \n\
    \  |_____| \\__,_||__|__||___,_||_____| \\___/ |__|__|     \\__,_| \
     \\__,_||_____| \\___|  |__|"
  in
  let _ = Sys.command "clear" in
  ANSITerminal.print_string [ ANSITerminal.blue ] ("\n\n" ^ titlestring ^ "\n\n");
  print_endline
    "Your adventure into the dungeons will soon begin. \n\
    \ \n\
    \ Please enter the number of the level you wish to load: \n\
    \ (1) Tomb of the Leviathan \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "1" -> play_game "level_tomb_of_the_leviathan"
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()
