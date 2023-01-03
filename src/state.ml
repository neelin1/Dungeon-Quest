type t = {
  mutable r : int;  (**current row*)
  mutable c : int;  (**current column*)
  mutable hp : int;
  mutable inv : string list;
}
(* Keeps track of the state of the player *)

let init_state r c = { r; c; hp = 100; inv = [ "sword"; "knife"; "potion" ] }
let current_row t = t.r
let current_col t = t.c
let current_hp t = t.hp

let change_coords player (r', c') =
  player.r <- r';
  player.c <- c';
  player

let update_hp i t = t.hp <- i

let update_inv item t =
  match item with
  | Some i ->
      t.inv <- i :: t.inv;
      Some i
  | None -> None

let update_coords player dr dc =
  let _ =
    let _ = player.r <- player.r + dr in
    player.c <- player.c + dc
  in
  player

let pp_string s = "\"" ^ s ^ "\""

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ ", ") t'
    in
    loop 0 "" lst
  in
  " " ^ pp_elts lst ^ " "

let inventory_to_string player =
  "Current Inventory: " ^ pp_list pp_string player.inv

let hp_to_string player = "Current HP: " ^ string_of_int player.hp
let get_inv_lst player = player.inv

(*Remove exactly one potion from player inventory.*)
let rec remove_potion inv prev =
  match inv with
  | [] -> prev
  | h :: t ->
      if h = "potion" then prev @ t
      else
        let prev' = prev @ [ h ] in
        remove_potion t prev'

let rec has_item str inv =
  match inv with
  | [] -> false
  | h :: t -> if h = str then true else has_item str t

let rec use_potion player inv =
  match inv with
  | [] -> print_string "No potion found in your inventory!"
  | h :: t ->
      if h = "potion" then (
        if player.hp + 20 > 100 then player.hp <- 100
        else player.hp <- player.hp + 20;
        player.inv <- remove_potion player.inv [];
        print_string ("You use the potion.\n" ^ hp_to_string player))
      else use_potion player t
