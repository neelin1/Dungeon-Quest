(** Represents the player*)

type t = {
  mutable r : int;
  mutable c : int;
  mutable hp : int;
  mutable inv : string list;
}
(** AR: Type representing the player; ref int for hp; int row for current row;
    int col for current column; string list for inventory *)

val init_state : int -> int -> t
(** [init_state r c] is the initial state of the player with row [r] and column
    [c] *)

val current_row : t -> int
(** [current_row player] is the current row of [player] in the level*)

val current_col : t -> int
(** [current_col player] is the column of [player] *)

val current_hp : t -> int
(** [current_hp t] is the hp of the player *)

val change_coords : t -> int * int -> t
(** [change_coords t (r,c)] is the player with row [r] and column [c] *)

val update_hp : int -> t -> unit
(** [update_hp hp t] is the player with hp [hp] *)

val update_inv : string option -> t -> string option
(** [update_inv str_o player] is the added string [str_o]; also updates the
    inventory to add the new item *)

val update_coords : t -> int -> int -> t
(** [update_coords player r c] is the player with coordinates changed to [r] and
    [c] *)

val pp_string : string -> string
(** [pp_string s] is the string [s] with the first letter capitalized *)

val pp_list : ('a -> string) -> 'a list -> string
(** [pp_list f lst] is the string representation of the list [lst] with the
    function [f] *)

val inventory_to_string : t -> string
(** [inventory_to_string t] is the string representation of the inventory of the
    player *)

val hp_to_string : t -> string
(** [hp_to_string t] is the string representation of the hp of the player *)

val get_inv_lst : t -> string list
(** [get_inv_lst t] is the inventory of the player *)

val remove_potion : string list -> string list -> string list
(** [remove_potion lst1 lst2] is the list [lst1] with the first element of
    [lst2] removed *)

val has_item : string -> string list -> bool
(** [has_itme str lst] is [true] is [lst] contains [str], otherwise [false] *)

val use_potion : t -> string list -> unit
(** [use_potion t lst] is the player with the first element of [lst] removed
    from the inventory *)
