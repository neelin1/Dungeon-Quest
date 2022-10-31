type t
(**abstract type representing the player*)

val init_state : int -> int -> t
(**Takes starting coordinates and creates initial hp, etc. for the player*)

val current_row : t -> int
(**current row of the player*)

val current_col : t -> int
(**current column of the player*)

val current_hp : t -> int
(**current hp of the player*)

(* val current_inv : t -> string list *)

val update_coords : t -> int -> int -> t
(**update current coordinates of the player*)

val inventory_to_string : t -> string
(**returns the players inventory as a string*)