open Base
open Stdio

type t = {
  path : string;
  name : string;
  config_file : string option;
}

let find_config_file path =
  let config_paths =
    [
      path ^ "/narratoric.json";
      path ^ "/narratoric.yaml";
      path ^ "/narratoric.toml";
      path ^ "/.narratoric/config.json";
    ]
  in
  List.find config_paths ~f:(fun p -> Stdlib.Sys.file_exists p)

let load path =
  let name = Stdlib.Filename.basename path in
  let config_file = find_config_file path in
  { path; name; config_file }

let validate project =
  let checks =
    [
      ("Project directory exists", Stdlib.Sys.file_exists project.path);
      ("Configuration file", Option.is_some project.config_file);
      ("Scenes directory", Stdlib.Sys.file_exists (project.path ^ "/scenes"));
    ]
  in
  checks

let init_structure path name template =
  let dirs =
    match template with
    | "visual-novel" -> [ "scenes"; "characters"; "backgrounds"; "music"; "sfx"; "saves" ]
    | "interactive-fiction" -> [ "chapters"; "choices"; "inventory"; "saves" ]
    | "custom" -> [ "src"; "assets"; "config" ]
    | _ -> [ "scenes"; "assets"; "scripts"; "config" ]
  in

  let project_path = path ^ "/" ^ name in

  (* Create project directory *)
  if not (Stdlib.Sys.file_exists project_path) then Unix.mkdir project_path 0o755;

  (* Create subdirectories *)
  List.iter dirs ~f:(fun dir ->
      let dir_path = project_path ^ "/" ^ dir in
      if not (Stdlib.Sys.file_exists dir_path) then Unix.mkdir dir_path 0o755 );

  (* Create default config *)
  let config_content =
    Printf.sprintf
      {|{
  "name": "%s",
  "version": "0.1.0",
  "template": "%s",
  "engine": {
    "version": "0.1.0"
  }
}|}
      name template
  in

  let config_path = project_path ^ "/narratoric.json" in
  Out_channel.write_all config_path ~data:config_content;

  project_path
