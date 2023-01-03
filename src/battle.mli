(** Represents the battle*)

type move =
  | UnarmedAttack
  | ArmedAttack
  | PrepareAttack
  | Defend
  | Flee
      (** AR: Type representing a player action during combat with Unarmed
          Attack, Armed Attack, Prepare Attack, Defend, and Flee*)

type enemy = {
  name : string;
  mutable hp : int;
  mutable armed : bool;
  mutable guarding : bool;
}
(** AR: Type representing the enemy with name, hp, armed, guarding*)

type player = {
  mutable hp : int;
  primary_weapon : string;
  mutable armed : bool;
  mutable guarding : bool;
}
(** AR: Type representing the enemy with hp, primary_weapon, armed, guarding RI:
    primary_weapon must be one of the valid types *)

type move_data = {
  text : string;
  dmg : int;
}
(** AR: Type representing the move data with text and dmg*)

type weapon_data = {
  name : string;
  aa_prepared : string;
  aa_unprepared : string;
  prepare : string;
  damage : int;
  endingtext : string;
}
(** AR: Type representing the weapon data with name, aa_prepared, aa_unprepared,
    prepare, damage, endingtext*)

type battledata = {
  ua_enemy_ng : move_data;
  ua_enemy_g : move_data;
  aa_enemy_ng : move_data;
  aa_enemy_g : move_data;
  prepare_enemy : move_data;
  guard_enemy : move_data;
  loss_message : move_data;
}
(** AR: Type representing the battle data with ua_enemy_ng, ua_enemy_g,
    aa_enemy_ng, aa_enemy_g, prepare_enemy, guard_enemy, loss_message*)

type battle = {
  enemy : enemy;
  player : player;
  data : battledata;
  weapon : weapon_data;
  fleeable : bool;
}
(** AR: Type representing the battle with enemy, player, data, weapon*)

val fetch_weapons_data : string -> Yojson.Basic.t list
(** [fetch_weapons_data file] creates a list of weapons from the data of weapons
    available*)

val process_weapons_data : Yojson.Basic.t list -> weapon_data list
(** [process_weapons_data data] creates a list of weapons from the data of
    weapons available*)

val find_damage : string -> weapon_data list -> int
(** [find_damage name weapons] finds the damage of the weapon with name [name]
    in the list of weapons [weapons]*)

val create_damage_list : string list -> weapon_data list -> 'a -> int list
(** [create_damage_list names weapons acc] creates a list of damage values
    corresponding to the list of weapon names [names] in the list of weapons
    [weapons]*)

val select_best_weapon : 'a list -> 'a -> int -> int -> int
(** [select_best_weapon lst acc max index] finds the index of the maximum value
    in the list [lst]*)

val best_weapon_data : string -> weapon_data list -> weapon_data
(** [best_weapon_data name weapons] finds the weapon with name [name] in the
    list of weapons [weapons]*)

val best_weapon : State.t -> string -> weapon_data
(** [best_weapon state name] finds the best weapon for the player in the state
    [state] with name [name]*)

val enemy_hp : string -> int
(** [enemy_hp name] finds the hp of the enemy with name [name]*)

val start_message : string -> string
(** [start_message name] finds the start message of the enemy with name [name]*)

val fetch_data : string -> Yojson.Basic.t list
(** [fetch_data file] creates a list of moves from the data of moves available*)

val process_data : Yojson.Basic.t list -> move_data list
(** [process_data data] creates a list of moves from the data of moves available*)

val create_battle_data : string -> battledata
(** [create_battle_data name] creates the battle data for the enemy with name
    [name]*)

val random_move : battle -> move
(** [random_move battle] creates a random move for the enemy in the battle
    [battle]*)

val enemy_move : battle -> unit
(** [enemy_move battle] creates a move for the enemy in the battle [battle]*)

val player_move : battle -> move -> unit
(** [player_move battle move] creates a move for the player in the battle
    [battle] with move [move]*)

val print_state : battle -> unit
(** [print_state battle] prints the state of the battle [battle]*)

val check_endgame : battle -> State.t -> bool -> bool
(** [check_endgame battle state win] checks if the game is over in the battle
    [battle] with state [state] and win [win]*)

val next : string -> battle -> State.t -> unit
(** [next name battle state] creates the next battle with enemy with name [name]
    in the battle [battle] with state [state]*)

val play_game : State.t -> string -> unit
(** [play_game state name] plays the game with state [state] and enemy with name
    [name]*)
