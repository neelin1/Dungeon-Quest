(** Represents the dungeon*)

type encounter = {
  e_type : string;
  json : string;
  repeatable : bool;
  mutable visited : bool;
}
(**Type for [encounter] represents the encounter type, the string title of the
   related json file, whether its mutable, and if it has been visited RI:
   [e_type] is either "flavor", "battle", "keydoor", or "cutscene"*)

type dungeon = {
  mutable map : string list;
  mutable encounters : (int * int * encounter) list;
  starting_coords : int * int;
}
(** Type for dungeons with the map, the list of encounters, and the starting
    coordinates*)

val start_coords : dungeon -> int * int
(**[start_coords dung] returns the starting coordinates held within [dung]*)

val encounter_list : Yojson.Basic.t list -> (int * int * encounter) list
(**[encounter_list yoj_list] is the list of encounters (their coordinates,
   types, and jsons) held within the list [yoj_list] **)

val from_json_encounters : Yojson.Basic.t -> (int * int * encounter) list
(**[from_json_encounters json] is the list of encounters held within the json
   [yojson]*)

val from_json_start : Yojson.Basic.t -> int * int
(**[from_json_start json] is the starting coordinates held within the json
   [yojson]*)

val from_txt : string -> dungeon
(**[from_txt str] returns a record that includes string list where each line of
   given txt file is a separate string in the list, a list of encounters from a
   json file, and the starting coords of the dungeon *)

val print_dungeon : State.t -> int -> dungeon -> unit
(**[print_dungeon player line dungeon] prints the dungeon with the player placed
   at the coordinates given by [player]*)

val update_dungeon_state : string -> State.t -> dungeon -> State.t
(**[update_dungeon_state str player dungeon] is the player's updated state based
   on the encounter held within [str.json] and [str.txt]; it also updates the
   dungeon's state*)

val update_coords : int * int -> State.t -> State.t
(**[update_coords (r, c) player] is the player's updated state based on the
   encounter held within [str.json] and [str.txt]*)

val cutscene_item : string -> string
(**[cutscene_item str] is the item held within the cutscene [str.json]*)

val check_encounter : State.t -> (int * int * encounter) list -> dungeon -> unit
(**[check_encounter player enc_list dung] checks if the coordinates [player] is
   at has an encounter at it within [enc_list]. If it is, it prints said
   encounter and updates [dung]'s and [player]'s state respectively *)

val check_valid_move : int -> int -> int -> dungeon -> bool
(** [check_valid_move r c line dungeon] checks if the [r] [c] are a valid place
    to move in the dungeon (a period '.' or a hashtag '#') *)

val move_player : State.t -> int -> int -> int -> dungeon -> bool -> unit
(**[move_player player dr dc line dungeon] updates [player]'s coords and prints
   out new dungeon with updated coordinates*)

val print_inventory : State.t -> unit
(**[print_inventory player] prints the inventory of the player into the terminal*)

val print_hp : State.t -> unit
(**[print_hp player] prints the hp of the player into the terminal*)

val next : string -> State.t -> int -> dungeon -> 'a
(**[next str player line dungeon] recursively checks for player input and either
   quits or calls [move_player] according*)

val play_game : string -> unit
(**[play_game dungeon] loads and prints the dungeon based off the file input
   [dungeon]*)
