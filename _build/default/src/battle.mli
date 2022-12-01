type move =
  | UnarmedAttack
  | ArmedAttack
  | PrepareAttack
  | Defend
  | Flee

type enemy = {
  name : string;
  mutable hp : int;
  unarmed_damage : int;
  armed_damage : int;
  mutable armed : bool;
  mutable guarding : bool;
}

type player = {
  mutable hp : int;
  primary_weapon : string;
  mutable armed : bool;
  mutable guarding : bool;
}

type battle = {
  enemy : enemy;
  player : player;
}

val random_move : battle -> move
val enemy_move : battle -> unit
val player_move : battle -> move -> unit
