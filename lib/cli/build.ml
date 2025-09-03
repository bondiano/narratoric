open Base
open Stdio

type config = {
  source_path : string;
  output_path : string;
  watch_mode : bool;
  verbose : bool;
}

let collect_scenes path =
  let scenes_dir = path ^ "/scenes" in
  if Stdlib.Sys.file_exists scenes_dir && Stdlib.Sys.is_directory scenes_dir then
    Stdlib.Sys.readdir scenes_dir |> Array.to_list
    |> List.filter ~f:(fun f ->
           String.is_suffix f ~suffix:".narratoric" || String.is_suffix f ~suffix:".json" )
  else []

let compile_scene scene_file output_dir =
  (* TODO: Actual scene compilation *)
  let scene_name = Stdlib.Filename.basename scene_file in
  printf "  Compiling: %s -> %s/%s\n" scene_name output_dir scene_name;
  true

let run config =
  printf "Building project at: %s\n" config.source_path;
  printf "Output directory: %s\n" config.output_path;

  if config.watch_mode then begin
    printf "Watch mode enabled. Watching for changes...\n";
    printf "Press Ctrl+C to stop.\n";
    (* TODO: Implement file watching *)
    `Ok 0
  end
  else begin
    if config.verbose then printf "[verbose] Starting build process...\n";

    (* Create output directory if it doesn't exist *)
    if not (Stdlib.Sys.file_exists config.output_path) then
      Unix.mkdir config.output_path 0o755;

    (* Collect and compile scenes *)
    let scenes = collect_scenes config.source_path in
    printf "Found %d scenes\n" (List.length scenes);

    let results =
      List.map scenes ~f:(fun scene ->
          compile_scene (config.source_path ^ "/scenes/" ^ scene) config.output_path )
    in

    let success = List.for_all results ~f:Fn.id in

    if success then begin
      printf "\nBuild completed successfully!\n";
      printf "Output written to: %s\n" config.output_path;
      `Ok 0
    end
    else begin
      printf "\nBuild failed with errors.\n";
      `Ok 1
    end
  end
