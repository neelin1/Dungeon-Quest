open OUnit2
open Game
open State
open Dungeon
open Battle
open Keydoor
open Cutscene

(** The plan for testing was to go through each of the modules and test
    important functions along with the data files used in the actual game
    instead of creating specific testing files. Primarily, glass box testing was
    used in order to make sure function were working as intended and all paths
    were followed. Additionally, to check specification, black box testing was
    used to make sure specification and functions were working as intended. This
    testing approach demonstrates the correctness of the system by going through
    all modules and testing functions, and focusing on helper functions to prove
    correctness of larger functions that utilized helpers. All portions of the
    test suite were done with Ounit. The unit half the testing only represents a
    fraction of the total testing that the team did. Because this is a game that
    relies on printing functions, much of the testing was manual. This game has
    been playtested many times to make sure that eveything is printing and
    running correctly. OUnit test cases tested a lot of the helper functions and
    state changes. Manual testing tested mostly the functions that printed to
    screen and made sure that the different modules were working together
    properly. Player state, dungeon state, and how they mutate were a big focus
    of the manual tests. A combination of OUnit test cases and thorough manual
    testing is a fantastic way to demonstrate the correctness of a system. Glass
    box OUnit test cases prove core functionality and are better at seeing
    inside a function. Manual testing almost acts black-box testing. I am acting
    as a player; I don't see how the system works, but I can see if there are
    any glitches or weird issues that arise from play. This pairing of black-box
    and glass-box is a very effetive method of findng bugs.**)

let pp_string s = "\"" ^ s ^ "\""
let test_dungeon = from_txt "test"
let dungeon = from_txt "level_tomb_of_the_leviathan"

let player =
  let coords = start_coords dungeon in
  init_state (fst coords) (snd coords)

let string_of_t = function
  | { r; c; hp; inv } ->
      string_of_int r ^ " " ^ string_of_int c ^ " " ^ string_of_int hp ^ " "
      ^ string_of_int (List.length inv)

let dungeon_tests =
  [
    ("Test for encounter_list" >:: fun _ -> assert_equal [] (encounter_list []));
    ( "Test for check_encounter" >:: fun _ ->
      assert_equal () (check_encounter player [] test_dungeon) );
    ( "Test for check_valid_move" >:: fun _ ->
      assert_equal (check_valid_move 1 1 1 test_dungeon) true );
    ( "Test for check_valid_move" >:: fun _ ->
      assert_equal (check_valid_move 1 2 1 test_dungeon) true );
    ( "Test for check_valid_move" >:: fun _ ->
      assert_equal (check_valid_move 1 3 1 test_dungeon) false );
    ( "Test for check_valid_move" >:: fun _ ->
      assert_equal (check_valid_move 1 4 1 test_dungeon) false );
    ( "Test for check_valid_move" >:: fun _ ->
      assert_equal (check_valid_move 1 5 1 test_dungeon) false );
    ( "Test for check_valid_move" >:: fun _ ->
      assert_equal (check_valid_move 1 6 1 test_dungeon) false );
    ( "Test for start_coords" >:: fun _ ->
      assert_equal (start_coords test_dungeon) (0, 0) );
    ( "Test for start_coords" >:: fun _ ->
      assert_equal (start_coords dungeon) (1, 2) );
    ( "Test for update_dungeon_state" >:: fun _ ->
      assert_equal ~printer:string_of_t
        { r = 8; c = 23; hp = 100; inv = [] }
        (update_dungeon_state "keydoor_tomb"
           { r = 0; c = 0; hp = 100; inv = [] }
           test_dungeon) );
    (let new_player = { r = 0; c = 0; hp = 100; inv = [] } in
     let _ = move_player new_player 0 1 1 test_dungeon false in
     "Test for move_player" >:: fun _ ->
     assert_equal ~printer:string_of_t
       { r = 0; c = 1; hp = 100; inv = [] }
       new_player);
  ]

let test_sword =
  {
    name = "sword";
    damage = 10;
    aa_prepared = "You slash your enemy with your sword.";
    aa_unprepared = "Your sword is heavy, raise it before you strike!";
    prepare = "You raise your sword.";
    endingtext =
      "You slash the monster one last time, and it falls to the ground. \
       Victory is yours.";
  }

let test_weapon_data =
  [
    test_sword;
    {
      name = "axe";
      damage = 20;
      aa_prepared = "You slash your enemy with your axe.";
      aa_unprepared = "Your axe is heavy, raise it before you strike!";
      prepare = "You raise your axe.";
      endingtext =
        "You slash the monster one last time, and it falls to the ground. \
         Victory is yours.";
    };
  ]

let battle_player =
  { hp = 100; primary_weapon = "sword"; armed = false; guarding = false }

let test_battle =
  {
    enemy = { name = "monster"; hp = 0; armed = false; guarding = false };
    player = battle_player;
    data = create_battle_data "test_battle";
    weapon = test_sword;
    fleeable = false;
  }

let test_battle_2 =
  {
    enemy = { name = "enemy"; hp = 100; armed = true; guarding = false };
    player = battle_player;
    data = create_battle_data "test_battle";
    weapon = test_sword;
    fleeable = false;
  }

let test_player = init_state 0 0

let string_of_weapon_data = function
  | { name; damage; aa_prepared; aa_unprepared; prepare; endingtext } ->
      name ^ " " ^ string_of_int damage ^ " " ^ aa_prepared ^ " "
      ^ aa_unprepared ^ " " ^ prepare ^ " " ^ endingtext

let weapon_data_list = [ test_sword ]

let battle_tests =
  [
    ( "Test for best_weapon" >:: fun _ ->
      assert_equal ~printer:string_of_weapon_data
        {
          name = "sword";
          damage = 10;
          aa_prepared = "You slash your enemy with your sword.";
          aa_unprepared = "Your sword is heavy, raise it before you strike!";
          prepare = "You raise your sword.";
          endingtext =
            "\n\
             You slash the monster one last time, and it falls to the ground.\n\
             Victory is yours.";
        }
        (best_weapon test_player "test_battle") );
    ( "Test for best_weapon_data" >:: fun _ ->
      assert_equal
        {
          name = "sword";
          damage = 10;
          aa_prepared = "bruh";
          aa_unprepared = "bruh";
          prepare = "bruh";
          endingtext = "bruh";
        }
        (best_weapon_data "name" []) );
    ( "Test for find damage1" >:: fun _ ->
      assert_equal ~printer:string_of_int 10
        (find_damage "sword" test_weapon_data) );
    ( "Test for find damage2" >:: fun _ ->
      assert_equal ~printer:string_of_int 20
        (find_damage "axe" test_weapon_data) );
    ( "Test for select_best_weapon1" >:: fun _ ->
      assert_equal ~printer:string_of_int 1
        (select_best_weapon [ 10; 20 ] 0 0 0) );
    ( "Test for select_best_weapon2" >:: fun _ ->
      assert_equal ~printer:string_of_int 0
        (select_best_weapon [ 10; 10 ] 0 0 0) );
    ( "Test for select_best_weapon2" >:: fun _ ->
      assert_equal ~printer:string_of_int 0 (select_best_weapon [] 0 0 0) );
    ( "Test for check_endgame" >:: fun _ ->
      assert_equal ~printer:string_of_bool false
        (check_endgame test_battle test_player false) );
    ( "Test for start_message" >:: fun _ ->
      assert_equal "A monster appears and attacks you."
        (start_message "test_battle") );
    ( "Test for create_damage_list" >:: fun _ ->
      assert_equal [] (create_damage_list [] [] []) );
    ( "Test for best_weapon_data" >:: fun _ ->
      assert_equal ~printer:string_of_weapon_data test_sword
        (best_weapon_data "sword" weapon_data_list) );
    ("Test for enemy_hp" >:: fun _ -> assert_equal 60 (enemy_hp "battle_spider"));
    ( "Test for enemy_hp" >:: fun _ ->
      assert_equal 200 (enemy_hp "battle_leviathan") );
    ( "Test for enemy_hp" >:: fun _ ->
      assert_equal ~printer:string_of_int 50 (enemy_hp "battle_zombie") );
    ( "Test for enemy_hp" >:: fun _ ->
      assert_equal ~printer:string_of_int 50 (enemy_hp "battle_test") );
    ( "Test for enemy_hp" >:: fun _ ->
      assert_equal ~printer:string_of_int 120
        (enemy_hp "battle_minotaurskeleton") );
  ]

let string_of_option = function
  | None -> "None"
  | Some x -> x

let state_tests =
  [
    ( "Test for current_row" >:: fun _ ->
      assert_equal ~printer:string_of_int (current_row test_player) 0 );
    ( "Test for current_col" >:: fun _ ->
      assert_equal ~printer:string_of_int (current_col test_player) 0 );
    ( "Test for current_hp" >:: fun _ ->
      assert_equal (current_hp test_player) 100 );
    ( "Test for update_hp" >:: fun _ ->
      assert_equal
        (test_player |> update_hp 150;
         current_hp test_player)
        150 );
    ( "Test for update_hp" >:: fun _ ->
      assert_equal
        (test_player |> update_hp (-1);
         current_hp test_player)
        (-1) );
    ( "Test for update_hp" >:: fun _ ->
      assert_equal
        (test_player |> update_hp 100;
         current_hp test_player)
        100 );
    ( "Test for update_inv" >:: fun _ ->
      assert_equal ~printer:string_of_option
        (update_inv (Some "item") test_player)
        (Some "item") );
    ( "Test for update_inv" >:: fun _ ->
      assert_equal (update_inv None test_player) None );
    ( "Test for update_inv" >:: fun _ ->
      assert_equal (update_inv (Some "") test_player) (Some "") );
    ( "Test for update_coords1" >:: fun _ ->
      assert_equal ~printer:string_of_int 1
        (current_col (update_coords (0, 1) test_player)) );
    ( "Test for update_coords3" >:: fun _ ->
      assert_equal ~printer:string_of_int 1
        (current_row (update_coords (1, 0) test_player)) );
    ( "Test for inventory_to_string" >:: fun _ ->
      assert_equal ~printer:pp_string
        "Current Inventory:  \"sword\", \"knife\", \"potion\" "
        (inventory_to_string player) );
    ( "Test for hp_to_string" >:: fun _ ->
      assert_equal ~printer:pp_string "Current HP: 100" (hp_to_string player) );
    ( "Test for change coords" >:: fun _ ->
      assert_equal
        { r = 1; c = 1; hp = 0; inv = [] }
        (change_coords { r = 0; c = 0; hp = 0; inv = [] } (1, 1)) );
    ( "Test for change coords" >:: fun _ ->
      assert_equal
        { r = 1; c = 1; hp = 0; inv = [] }
        (change_coords { r = 1; c = 1; hp = 0; inv = [] } (1, 1)) );
    ( "Test for remove_potion" >:: fun _ ->
      assert_equal [] (remove_potion [ "potion" ] []) );
    ("Test for remove_potion" >:: fun _ -> assert_equal [] (remove_potion [] []));
    ( "Test for remove_potion" >:: fun _ ->
      assert_equal [ "non-potion" ]
        (remove_potion [ "potion"; "non-potion" ] []) );
    ( "Test for remove_potion" >:: fun _ ->
      assert_equal [ "potion" ] (remove_potion [ "potion"; "potion" ] []) );
    ( "Test for get_inv_list" >:: fun _ ->
      assert_equal [] (get_inv_lst { r = 0; c = 0; hp = 100; inv = [] }) );
    ( "Test for get_inv_list" >:: fun _ ->
      assert_equal
        [ "crank"; "sword"; "flame scepter" ]
        (get_inv_lst
           {
             r = 0;
             c = 0;
             hp = 100;
             inv = [ "crank"; "sword"; "flame scepter" ];
           }) );
    ( "Test for has_potion" >:: fun _ ->
      assert_equal ~printer:string_of_bool false (has_item "strength-potion" [])
    );
    ( "Test for has_potion" >:: fun _ ->
      assert_equal ~printer:string_of_bool true (has_item "potion" [ "potion" ])
    );
    ( "Test for has_potion" >:: fun _ ->
      assert_equal ~printer:string_of_bool false
        (has_item "potion" [ "no potion" ]) );
  ]

let bridge_scene =
  {
    name = "the bridge";
    description = "the bridge";
    item = None;
    options = Quit;
  }

let tower_scene =
  {
    name = "the tower";
    description = "the tower";
    item = Some "item";
    options = Quit;
  }

let test_scene_list = [ tower_scene; bridge_scene ]

let string_of_scene = function
  | { name; description; item; options } ->
      name ^ description ^ string_of_option item

let cutscene_tests =
  [
    ( "Test for from_json_scenes" >:: fun _ ->
      assert_equal [] (from_json_scenes [] { r = 0; c = 0; hp = 100; inv = [] })
    );
    ( "Test for find_scene" >:: fun _ ->
      assert_raises (Failure "Not Found") (fun _ -> find_scene "test" []) );
    ( "Test for find_scene" >:: fun _ ->
      assert_equal ~printer:string_of_scene bridge_scene
        (find_scene "the bridge" [ bridge_scene ]) );
    ( "Test for find_scene" >:: fun _ ->
      assert_equal tower_scene
        (find_scene "the tower" [ tower_scene; bridge_scene ]) );
  ]

let test_keydoor_json =
  {|{
  "open_scene" : "You put the crank in the door and turn it three times to the right with all your might. \nA long corridor appears behind the door.\n - continue",
  "close_scene" : "The door won't budge, but there appears to be a gear-shaped keyhole. \n - continue",
  "key" : "crank",
  "end_floor": {
      "same": false,
      "name": "level_tomb",
      "r": 8,
      "c": 23
    }
}|}

let keydoordata =
  {
    openmsg =
      "You put the crank in the door and turn it three times to the right with \
       all your might. \n\
       A long corridor appears behind the door.\n\
      \ - continue";
    closedmsg =
      "The door won't budge, but there appears to be a gear-shaped keyhole. \n\
      \ - continue";
    key = "crank";
  }

let keydoordata2 =
  { openmsg = "key-message"; closedmsg = "ket-message"; key = "crank" }

let keydoor_tests =
  [
    ( "Test for fetch_keydoordata" >:: fun _ ->
      assert_equal keydoordata (fetch_keydoordata "keydoor_tomb") );
    ( "Test for has_key" >:: fun _ ->
      assert_equal ~printer:string_of_bool false (has_key keydoordata []) );
    ( "Test for has_key" >:: fun _ ->
      assert_equal ~printer:string_of_bool true
        (has_key keydoordata [ "crank" ]) );
    ( "Test for has_key" >:: fun _ ->
      assert_equal ~printer:string_of_bool true
        (has_key keydoordata [ "crank"; "notcrank" ]) );
    ( "Test for has_key" >:: fun _ ->
      assert_equal ~printer:string_of_bool false
        (has_key keydoordata [ "notcrank" ]) );
    ( "Test for play_game_keydoor" >:: fun _ ->
      assert_equal ~printer:string_of_bool false
        (Keydoor.play_game "keydoor_tomb"
           { r = 0; c = 0; hp = 100; inv = [] }
           false) );
  ]

let tests =
  "final project test suite"
  >::: List.flatten
         [
           dungeon_tests;
           state_tests;
           battle_tests;
           cutscene_tests;
           keydoor_tests;
         ]

let _ = run_test_tt_main tests
