open Dungeon

open State
(** Get inventory values from state.ml May need to add things to state.ml Lists
    options: "unarmed strike," "sword," "defend," "wield" unarmed strike =
    unarmed strike, nothing sword > unarmed strike, more damage sword = sword,
    nothing defend > unarmed strike & sword, nothing unarmed strike > wield,
    nothing have your own print system for putting battles out in utop **)

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

(** by generating in range 0 to 3 inc, this accounts for the 2x likelihood**)
let random_move battle =
  let x = Random.int 4 in
  match x with
  | 0 -> UnarmedAttack
  | 1 -> Defend
  | _ -> if battle.enemy.armed = true then ArmedAttack else PrepareAttack

let enemy_move battle =
  let x = random_move battle in
  match x with
  | UnarmedAttack ->
      battle.player.hp <- battle.player.hp - 2;
      print_endline "The monster scratches you. It stings."
  | ArmedAttack ->
      battle.player.hp <- battle.player.hp - 10;
      print_endline
        "The monster knocks you to the ground with its fiery breath."
  | PrepareAttack ->
      battle.enemy.armed <- true;
      print_endline "The monster rears its head, preparing for an attack."
  | Defend ->
      battle.enemy.guarding <- true;
      print_endline
        "The monster guards itself carefully to protect from an incoming \
         strike."
  | Flee -> ()

(** filler values for text and damage dealt**)
let player_move battle move =
  match move with
  | UnarmedAttack ->
      battle.enemy.hp <- battle.enemy.hp - 2;
      print_endline "You punch your enemy.";
      battle.enemy.guarding <- false
  | PrepareAttack ->
      print_endline "You raise your sword.";
      battle.player.armed <- true
  | ArmedAttack ->
      if battle.player.armed = true then (
        print_endline "You slash your enemy with your sword.";
        battle.enemy.hp <- battle.enemy.hp - 10;
        battle.enemy.guarding <- false)
      else print_endline "Your sword is heavy, raise it before you strike!"
  | Defend ->
      battle.player.guarding <- true;
      print_endline "You put up your guard in anticipation of attack."
  | Flee -> battle.player.guarding <- true

(** i don't understand why this says unused variable **)