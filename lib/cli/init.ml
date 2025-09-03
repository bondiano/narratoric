open Base
open Stdio

let available_templates =
  [
    ("basic", "Basic narrative game template");
    ("visual-novel", "Visual novel with character sprites and backgrounds");
    ("interactive-fiction", "Text-based interactive fiction");
    ("custom", "Empty project with minimal structure");
  ]

let run name template verbose path =
  printf "Initializing new project: %s\n" name;
  printf "Template: %s\n" template;

  if verbose then printf "[verbose] Creating project structure...\n";

  (* Check if template exists *)
  let template_valid =
    List.exists available_templates ~f:(fun (t, _) -> String.equal t template)
  in
  if not template_valid then begin
    printf "Error: Unknown template '%s'\n" template;
    printf "Available templates:\n";
    List.iter available_templates ~f:(fun (t, desc) -> printf "  - %s: %s\n" t desc);
    `Ok 1
  end
  else begin
    try
      (* Create project structure *)
      let project_path = path ^ "/" ^ name in

      (* Create directories based on template *)
      let dirs =
        match template with
        | "visual-novel" ->
            [ "scenes"; "characters"; "backgrounds"; "music"; "sfx"; "saves" ]
        | "interactive-fiction" -> [ "chapters"; "choices"; "inventory"; "saves" ]
        | "custom" -> [ "src"; "assets"; "config" ]
        | _ -> [ "scenes"; "assets"; "scripts"; "config" ]
      in

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

      printf "\n✨ Project '%s' initialized successfully!\n" name;
      printf "\nProject structure:\n";

      (* List created directories *)
      let dirs = Stdlib.Sys.readdir project_path |> Array.to_list in
      List.iter dirs ~f:(fun dir ->
          if Stdlib.Sys.is_directory (project_path ^ "/" ^ dir) then
            printf "  📁 %s/\n" dir
          else printf "  📄 %s\n" dir );

      printf "\nTo get started:\n";
      printf "  cd %s\n" name;
      printf "  narratoric check\n";
      printf "  narratoric run --dev\n";

      `Ok 0
    with
    | Unix.Unix_error (error, func, arg) ->
        printf "Error creating project: %s in %s(%s)\n" (Unix.error_message error) func
          arg;
        `Ok 1
    | e ->
        printf "Error creating project: %s\n" (Exn.to_string e);
        `Ok 1
  end
