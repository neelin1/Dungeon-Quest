(** Represents a keydoor *)

open Yojson.Basic.Util
open State

type keydoordata = {
  openmsg : string;
  closedmsg : string;
  key : string;
}

let fetch_keydoordata file =
  let json =
    Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ file ^ ".json")
  in
  {
    openmsg = to_string (List.assoc "open_scene" (to_assoc json));
    closedmsg = to_string (List.assoc "close_scene" (to_assoc json));
    key = to_string (List.assoc "key" (to_assoc json));
  }

let rec next str =
  match str with
  | "continue" ->
      let _ = Sys.command "clear" in
      ()
  | "quit" | "q" ->
      let _ = Sys.command "clear" in
      let _ = print_endline "\n Goodbye!" in
      exit 0
  | _ ->
      print_endline "Please enter a valid command.";
      next (read_line ())

let rec has_key kdd lst =
  match lst with
  | [] -> false
  | h :: t -> if h = kdd.key then true else has_key kdd t

let play_game file (t : t) test =
  try
    let kdd = fetch_keydoordata file in
    let hk = has_key kdd (get_inv_lst t) in
    (if test then
     let _ =
       if hk then print_endline kdd.openmsg else print_endline kdd.closedmsg
     in
     next (read_line ()));
    hk
  with Sys_error _ -> failwith "No such keydoor"