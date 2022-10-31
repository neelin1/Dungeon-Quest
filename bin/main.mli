val player : Game.State.t
val print_dungeon : Game.State.t -> int -> string list -> unit
val move_player : Game.State.t -> int -> int -> int -> string list -> unit
val next : string -> Game.State.t -> int -> string list -> 'a
val play_game : string -> unit
val main : unit -> unit
