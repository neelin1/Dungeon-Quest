(** Represents the battle*)

open Yojson.Basic.Util

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
  mutable armed : bool;
  mutable guarding : bool;
}

type player = {
  mutable hp : int;
  primary_weapon : string;
  mutable armed : bool;
  mutable guarding : bool;
}

type move_data = {
  text : string;
  dmg : int;
}

type weapon_data = {
  name : string;
  aa_prepared : string;
  aa_unprepared : string;
  prepare : string;
  damage : int;
  endingtext : string;
}

type battledata = {
  ua_enemy_ng : move_data;
  ua_enemy_g : move_data;
  aa_enemy_ng : move_data;
  aa_enemy_g : move_data;
  prepare_enemy : move_data;
  guard_enemy : move_data;
  loss_message : move_data;
}

type battle = {
  enemy : enemy;
  player : player;
  data : battledata;
  weapon : weapon_data;
  fleeable : bool;
}

(* PARSING WEAPONS.JSON FOR WEAPONS DATA & SELECTING MAX DAMAGE WEAPON *)
let fetch_weapons_data file =
  to_list
    (List.assoc "weapons"
       (to_assoc
          (Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ file ^ ".json"))))

(* Here, we have all the data from the weapons file on all of the different
   weapons in the dungeon. *)
let rec process_weapons_data lst =
  match lst with
  | [] -> []
  | h :: t ->
      {
        name = to_string (List.assoc "name" (to_assoc h));
        damage = to_int (List.assoc "damage" (to_assoc h));
        aa_prepared = to_string (List.assoc "armedattack_prepared" (to_assoc h));
        aa_unprepared =
          to_string (List.assoc "armedattack_unprepared" (to_assoc h));
        prepare = to_string (List.assoc "prepare" (to_assoc h));
        endingtext = to_string (List.assoc "endingtext" (to_assoc h));
      }
      :: process_weapons_data t

(* Now, we need to fetch our inventory. For each weapon in the inventory, if
   found, we append the damage to the list. Otherwise append 0. Then, we will
   write another function to find the maximum index of the list. This will allow
   us to select the final weapon_data record to use in our battledata.*)
let rec find_damage weapon wd_lst =
  match wd_lst with
  | [] -> 0
  | h :: t -> if h.name = weapon then h.damage else find_damage weapon t

let rec create_damage_list inv_lst wd_lst acc =
  match inv_lst with
  | [] -> []
  | h_inv :: t_inv ->
      find_damage h_inv wd_lst :: create_damage_list t_inv wd_lst acc

(* This gives us the index of the highest integer in the int list. This will
   correspond to the inventory lst. *)
(* max will start at 0, max_index at 0, cur_index at 0 *)
let rec select_best_weapon dmg_lst max max_index cur_index =
  match dmg_lst with
  | [] -> max_index
  | h :: t ->
      if h > max then select_best_weapon t h cur_index (cur_index + 1)
      else select_best_weapon t max max_index (cur_index + 1)

let rec best_weapon_data name wd_lst =
  match wd_lst with
  | [] ->
      {
        name = "sword";
        damage = 10;
        aa_prepared = "bruh";
        aa_unprepared = "bruh";
        prepare = "bruh";
        endingtext = "bruh";
      }
      (*default if not found is sword*)
  | h :: t -> if h.name = name then h else best_weapon_data name t

(* This function will compress all of the above helpers into one function to
   make our lives easier.*)
let best_weapon player file =
  let weapon_data_list = process_weapons_data (fetch_weapons_data file) in
  let index_best =
    select_best_weapon
      (create_damage_list (get_inv_lst player) weapon_data_list [])
      0 0 0
  in
  let weapon_name = List.nth (get_inv_lst player) index_best in
  best_weapon_data weapon_name weapon_data_list

let enemy_hp file =
  to_int
    (List.assoc "hp"
       (to_assoc
          (Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ file ^ ".json"))))

let start_message file =
  to_string
    (List.assoc "start message"
       (to_assoc
          (Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ file ^ ".json"))))

let fleeable file =
  to_bool
    (List.assoc "fleeable"
       (to_assoc
          (Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ file ^ ".json"))))

let fetch_data file =
  to_list
    (List.assoc "moves"
       (to_assoc
          (Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ file ^ ".json"))))

let rec process_data lst =
  match lst with
  | [] -> []
  | h :: t ->
      {
        text = to_string (List.assoc "text" (to_assoc h));
        dmg = to_int (List.assoc "damage" (to_assoc h));
      }
      :: process_data t

let create_battle_data file =
  let datalst = process_data (fetch_data file) in
  {
    ua_enemy_ng = List.nth datalst 0;
    ua_enemy_g = List.nth datalst 1;
    aa_enemy_ng = List.nth datalst 2;
    aa_enemy_g = List.nth datalst 3;
    prepare_enemy = List.nth datalst 4;
    guard_enemy = List.nth datalst 5;
    loss_message = List.nth datalst 6;
  }

(** by generating in range 0 to 3 inc, this accounts for the 2x likelihood**)
let random_move battle =
  let x = Random.int 4 in
  match x with
  | 0 -> UnarmedAttack
  | 1 -> if battle.enemy.guarding = true then UnarmedAttack else Defend
  | _ -> if battle.enemy.armed = true then ArmedAttack else PrepareAttack

let enemy_move battle =
  let x = random_move battle in
  match x with
  | UnarmedAttack ->
      if battle.player.guarding = true then (
        print_endline battle.data.ua_enemy_g.text;
        battle.player.guarding <- false)
      else (
        battle.player.hp <- battle.player.hp - battle.data.ua_enemy_ng.dmg;
        print_endline battle.data.ua_enemy_ng.text)
  | ArmedAttack ->
      if battle.player.guarding = true then (
        battle.player.hp <- battle.player.hp - battle.data.aa_enemy_g.dmg;
        print_endline battle.data.aa_enemy_g.text;
        battle.player.guarding <- false;
        battle.enemy.armed <- false)
      else (
        battle.player.hp <- battle.player.hp - battle.data.aa_enemy_ng.dmg;
        print_endline battle.data.aa_enemy_ng.text;
        battle.enemy.armed <- false)
  | PrepareAttack ->
      battle.enemy.armed <- true;
      print_endline battle.data.prepare_enemy.text
  | Defend ->
      battle.enemy.guarding <- true;
      print_endline battle.data.guard_enemy.text
  | Flee -> ()

(** filler values for text and damage dealt**)
let player_move battle move =
  match move with
  | UnarmedAttack ->
      if battle.enemy.guarding = true then (
        print_endline
          "You attack with your knife, but the monster manages to block and \
           avoid it. Its gaurd comes down.";
        battle.enemy.guarding <- false)
      else (
        battle.enemy.hp <- battle.enemy.hp - 5;
        print_endline
          "You attack with your knife and catch the monster off-guard. It is \
           briefly stunned.")
  | PrepareAttack ->
      print_endline battle.weapon.prepare;
      battle.player.armed <- true
  | ArmedAttack ->
      if battle.player.armed = true then (
        print_endline battle.weapon.aa_prepared;
        battle.enemy.hp <- battle.enemy.hp - battle.weapon.damage;
        battle.enemy.guarding <- false;
        battle.player.armed <- false)
      else print_endline battle.weapon.aa_unprepared
  | Defend ->
      battle.player.guarding <- true;
      print_endline "You put up your guard in anticipation of attack."
  | Flee -> print_endline "You run away as fast as you can."

let print_state game =
  print_endline
    "OPTIONS: main attack | basic attack | load | defend | inventory | potion \
     | flee";
  print_endline
    ("HP = "
    ^ string_of_int game.player.hp
    ^ " | ENEMY HP = "
    ^ string_of_int game.enemy.hp
    ^ " | ARMED = "
    ^ string_of_bool game.player.armed
    ^ " | DEFENDING = "
    ^ string_of_bool game.player.guarding)

let check_endgame game t test =
  if game.player.hp <= 0 then
    let _ = print_endline game.data.loss_message.text in
    exit 0
  else if game.enemy.hp <= 0 then
    let _ =
      (* Endgame behavior *)
      let _ = update_inv (Some "potion") t in
      ();
      update_hp game.player.hp t;
      if test then print_endline game.weapon.endingtext
    in
    false
  else true

let rec next str game t =
  let _ = Sys.command "clear" in
  match str with
  | "main attack" ->
      player_move game ArmedAttack;
      enemy_move game;
      print_state game;
      if check_endgame game t true then next (read_line ()) game t
  | "basic attack" ->
      player_move game UnarmedAttack;
      enemy_move game;
      print_state game;
      if check_endgame game t true then next (read_line ()) game t
  | "load" ->
      player_move game PrepareAttack;
      enemy_move game;
      print_state game;
      if check_endgame game t true then next (read_line ()) game t
  | "defend" ->
      player_move game Defend;
      enemy_move game;
      print_state game;
      if check_endgame game t true then next (read_line ()) game t
  | "inventory" ->
      print_endline (inventory_to_string t ^ "\n");
      print_state game;
      if check_endgame game t true then next (read_line ()) game t
  | "potion" ->
      if has_item "potion" (get_inv_lst t) then (
        print_endline "You use the potion.";
        t.inv <- remove_potion t.inv [];
        if game.player.hp + 20 > 100 then game.player.hp <- 100
        else game.player.hp <- game.player.hp + 20)
      else print_endline "You don't have a potion to use!";
      enemy_move game;
      print_state game;
      if check_endgame game t true then next (read_line ()) game t
  | "flee" ->
      if game.fleeable then
        let r = Random.int 5 in
        if r = 1 then (
          update_hp game.player.hp t;
          player_move game Flee)
        else (
          print_endline "You cannot flee!";
          enemy_move game;
          print_state game;
          if check_endgame game t true then next (read_line ()) game t)
      else (
        print_endline "You cannot flee!";
        enemy_move game;
        print_state game;
        if check_endgame game t true then next (read_line ()) game t)
  | "godflee" ->
      update_hp game.player.hp t;
      player_move game Flee
  | "quit" | "q" ->
      let _ = print_endline "\n Goodbye!" in
      exit 0
  | _ ->
      print_endline "Please enter a valid command.";
      print_state game;
      next (read_line ()) game t

let play_game t file =
  try
    let battledata = create_battle_data file in
    let enemy =
      { name = "Monster"; hp = enemy_hp file; armed = false; guarding = false }
    in
    let player =
      {
        hp = current_hp t;
        primary_weapon = "sword";
        armed = false;
        guarding = false;
      }
    in
    let wd = best_weapon t file in
    let game =
      {
        enemy;
        player;
        data = battledata;
        weapon = wd;
        fleeable = fleeable file;
      }
    in
    let _ =
      print_endline ("\n" ^ start_message file ^ "\n");
      print_state game
    in
    next (read_line ()) game t
  with Sys_error _ -> failwith "Encounter cutscene does not exist"