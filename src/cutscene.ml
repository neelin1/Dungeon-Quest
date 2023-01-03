open Yojson.Basic.Util
open State

type exits =
  | Return of int * int
  | ExitList of (string * string) list
  | Quit

type scene = {
  name : string;
  description : string;
  item : string option;
  options : exits;
}

let rec from_json_scenes lst ply =
  match lst with
  | [] -> []
  | h :: t ->
      {
        name = to_string (List.assoc "name" (to_assoc h));
        description = to_string (List.assoc "description" (to_assoc h));
        item =
          (try Some (to_string (List.assoc "item" (to_assoc h)))
           with Not_found -> None);
        options =
          (try
             ExitList
               (List.map
                  (fun s ->
                    ( to_string (List.assoc "name" (to_assoc s)),
                      to_string (List.assoc "id" (to_assoc s)) ))
                  (to_list (List.assoc "exits" (to_assoc h))))
           with Not_found ->
             let exit = to_string (List.assoc "description" (to_assoc h)) in
             if String.sub exit 0 4 = "exit" then
               let coords =
                 String.split_on_char ','
                   (String.sub exit 4 (String.length exit - 4))
               in
               Return
                 ( int_of_string (List.hd coords),
                   int_of_string (List.nth coords 1) )
             else Quit);
      }
      :: from_json_scenes t ply

let rec find_scene str scenes =
  match scenes with
  | [] -> failwith "Not Found"
  | ({ name = id; description = _; options = _ } as s) :: t ->
      (* let _ = print_endline id in *)
      if id = str then s else find_scene str t

let from_json_start json scenes =
  find_scene (to_string (List.assoc "start scene" (to_assoc json))) scenes

let rec next_scene (str : string) (scenes : scene list) (s : scene)
    (exit_list : (string * string) list) (ply : t) : int * int =
  try
    let exit_s = List.assoc str exit_list in
    try
      let exit_scene = find_scene exit_s scenes in
      match exit_scene.options with
      | ExitList lst ->
          ignore (Sys.command "clear");
          ignore (update_inv exit_scene.item ply);
          print_endline ("\n" ^ exit_scene.description ^ "\n");
          next (read_line ()) scenes exit_scene ply
      | Return (r, c) ->
          let _ = Sys.command "clear" in
          print_endline "";
          (r, c)
      | Quit -> exit 0
    with Failure _ ->
      let _ = print_endline "\n Please enter a valid option. \n" in
      next (read_line ()) scenes s ply
  with Not_found ->
    let _ = print_endline "\n Please enter a valid option. \n" in
    next (read_line ()) scenes s ply

and next str scenes s ply =
  match s.options with
  | ExitList lst -> next_scene str scenes s lst ply
  | Return _ -> failwith "exit called in wrong place"
  | Quit -> failwith "exit called in wrong place"

let play_game file ply : int * int =
  try
    let scenes =
      from_json_scenes
        (to_list
           (List.assoc "scenes"
              (to_assoc
                 (Yojson.Basic.from_file
                    ("data" ^ Filename.dir_sep ^ file ^ ".json")))))
        ply
    in

    let start_room =
      from_json_start
        (Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ file ^ ".json"))
        scenes
    in
    let _ = print_endline ("\n" ^ start_room.description ^ "\n") in
    next (read_line ()) scenes start_room ply
  with Sys_error _ -> failwith "Encounter cutscene does not exist"