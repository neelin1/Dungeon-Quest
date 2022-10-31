type t = {
  mutable r : int;  (**current row*)
  mutable c : int;  (**current column*)
  hp : int ref;
  mutable inv : string list;
}
(**player representation: ref int for hp; int row for current row; int col for
   current column; string list for inventory *)

let init_state r c = { r; c; hp = ref 100; inv = [ "sword"; "knife" ] }
let current_row t = t.r
let current_col t = t.c
let current_hp t = !(t.hp)
(* let current_inv player = player.inv *)

let update_coords player dr dc =
  let _ =
    let _ = player.r <- player.r + dr in
    player.c <- player.c + dc
  in
  player

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
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
