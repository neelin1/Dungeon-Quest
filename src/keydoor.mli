(** Represents a keydoor *)

type keydoordata = {
  openmsg : string;
  closedmsg : string;
  key : string;
}
(** Type for the keydoor with the open message, closed message, and key*)

val fetch_keydoordata : string -> keydoordata
(** [fetch_keydoordata s] is the keydoordata for the keydoor with the name [s]. *)

val next : string -> unit
(** [next s] is the next keydoor in the game. *)

val has_key : keydoordata -> string list -> bool
(** [has_key kd keys] is true if the keydoor [kd] is in the list of keys [keys]. *)

val play_game : string -> State.t -> bool -> bool
(** [play_game s st b] is the result of playing the game with the name [s] and
    the state [st]. *)
