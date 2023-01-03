(** Represents a cutscene*)

type exits =
  | Return of int * int
  | ExitList of (string * string) list
  | Quit
      (** AR: Type of exits with Return, List of Exits, and Quit RI: Return
          (r,c) is (0,0) if the player does not change coordinates during the
          cutscene; otherwise it is the coordiantes the player goes to after the
          cutscene is over*)

type scene = {
  name : string;
  description : string;
  item : string option;
  options : exits;
}
(**AR: Type of [scene] is the name of the scene, its description, if it returns
   an item, and it exit options RI: description must be longer than 4
   characters; the starting scene must not be an exit or quit*)

val from_json_scenes : Yojson.Basic.t list -> State.t -> scene list
(**[from_json_scenes json_list] returns a list of scenes from the json list
   [json_list]*)

val find_scene : string -> scene list -> scene
(**[find_scene name scene_list] is the scene with the name [name] from
   [scene_list]; if not found it fails with [Not_found]*)

val from_json_start : Yojson.Basic.t -> scene list -> scene
(**[from_json_start json scene_list] returns the starting scene from the json
   [json] and the scene list [scene_list]*)

val next_scene :
  string ->
  scene list ->
  scene ->
  (string * string) list ->
  State.t ->
  int * int
(**[next_scene name scene_list scene options state] is the int coordinate pair
   returns by an exit it call; it also prints the next scene from the scene list
   [scene_list] that correspond with [name] and the options [options]*)

val next : string -> scene list -> scene -> State.t -> int * int
(**[next name scene_list scene state] either calls [next_scene] on the next
   scene from the scene list [scene_list] and the scene [scene] with the name
   [name] or fails*)

val play_game : string -> State.t -> int * int
(**[play_game file_name state] plays the game with the file name [file_name] and
   the state [state] and returns the coordinates of an exit (if the player
   doesn't quit)*)
