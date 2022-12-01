type dungeon

val start_coords : dungeon -> int * int
(** [start_coords game] returns the starting coords of [game]. *)

val play_game : string -> unit
(** [play_game text] starts the dungeon in file [text]. *)