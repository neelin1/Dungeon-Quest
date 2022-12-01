open Yojson.Basic.Util

type exits =
  | Return
  | ExitList of (string * string) list
  | Quit

type scene = {
  name : string;
  description : string;
  options : exits;
}

let rec from_json_scenes lst =
  match lst with
  | [] -> []
  | h :: t ->
      {
        name = to_string (List.assoc "name" (to_assoc h));
        description = to_string (List.assoc "description" (to_assoc h));
        options =
          (try
             ExitList
               (List.map
                  (fun s ->
                    ( to_string (List.assoc "name" (to_assoc s)),
                      to_string (List.assoc "id" (to_assoc s)) ))
                  (to_list (List.assoc "exits" (to_assoc h))))
           with Not_found ->
             if to_string (List.assoc "description" (to_assoc h)) = "exit" then
               Return
             else Quit);
      }
      :: from_json_scenes t

let rec find_scene str scenes =
  match scenes with
  | [] -> failwith "Not Found"
  | ({ name = id; description = _; options = _ } as s) :: t ->
      (* let _ = print_endline id in *)
      if id = str then s else find_scene str t

let from_json_start json scenes =
  find_scene (to_string (List.assoc "start scene" (to_assoc json))) scenes

let rec print_exits = function
  | [] -> ""
  | h :: t -> ("\n - " ^ h) ^ print_exits t

let rec next_scene (str : string) (scenes : scene list) (s : scene)
    (exit_list : (string * string) list) =
  try
    let exit_s = List.assoc str exit_list in
    try
      let exit_scene = find_scene exit_s scenes in
      match exit_scene.options with
      | ExitList lst ->
          let _ = Sys.command "clear" in
          let _ = print_endline ("\n" ^ exit_scene.description ^ "\n") in
          next (read_line ()) scenes exit_scene
      | Return -> print_endline ""
      | Quit -> exit 0
    with Failure _ ->
      let _ = print_endline "\n Please enter a valid option. \n" in
      next (read_line ()) scenes s
  with Not_found ->
    let _ = print_endline "\n Please enter a valid option. \n" in
    next (read_line ()) scenes s

and next str scenes s =
  match s.options with
  | ExitList lst -> next_scene str scenes s lst
  | Return -> failwith "exit called in wrong place"
  | Quit -> failwith "exit called in wrong place"

let play_game file =
  try
    let scenes =
      from_json_scenes
        (to_list
           (List.assoc "scenes"
              (to_assoc
                 (Yojson.Basic.from_file
                    ("data" ^ Filename.dir_sep ^ file ^ ".json")))))
    in

    let start_room =
      from_json_start
        (Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ file ^ ".json"))
        scenes
    in
    let _ = print_endline ("\n" ^ start_room.description ^ "\n") in
    next (read_line ()) scenes start_room
  with Sys_error _ -> failwith "Encounter cutscene does not exist"