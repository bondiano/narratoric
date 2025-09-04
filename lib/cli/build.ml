open Base
open Stdio
module Story = Narratoric.Story

type config = {
  source_path : string;
  output_path : string;
  watch_mode : bool;
  verbose : bool;
}

let collect_story_files path =
  let files = ref [] in
  let rec collect_dir dir =
    if Stdlib.Sys.file_exists dir && Stdlib.Sys.is_directory dir then
      Stdlib.Sys.readdir dir
      |> Array.iter ~f:(fun f ->
             let full_path = dir ^ "/" ^ f in
             if String.is_suffix f ~suffix:".story" then files := full_path :: !files
             else if
               Stdlib.Sys.is_directory full_path
               && (not (String.equal f "."))
               && not (String.equal f "..")
             then collect_dir full_path )
  in
  collect_dir path;
  List.rev !files

let compile_story_file story_file output_dir =
  try
    let content = In_channel.read_all story_file in
    let tokens = Story.Lexer.lex_story content in

    match Story.Parser.parse tokens with
    | Ok story_ast ->
        let base_name =
          Stdlib.Filename.basename story_file |> Stdlib.Filename.chop_extension
        in
        let story_id = String.substr_replace_all base_name ~pattern:"-" ~with_:"_" in
        let js_code = Story.Codegen.compile_to_js ~story_id story_ast in
        let output_file = output_dir ^ "/" ^ base_name ^ ".js" in

        Out_channel.write_all output_file ~data:js_code;
        printf "  ✓ Compiled: %s -> %s\n"
          (Stdlib.Filename.basename story_file)
          (Stdlib.Filename.basename output_file);
        true
    | Error parse_error ->
        printf "  ✗ Parse error in %s: %s\n" story_file parse_error.message;
        false
  with exn ->
    printf "  ✗ Compilation error in %s: %s\n" story_file (Exn.to_string exn);
    false

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

    (* Collect and compile story files *)
    let story_files = collect_story_files config.source_path in
    printf "Found %d .story files\n" (List.length story_files);

    let results =
      List.map story_files ~f:(fun story_file ->
          compile_story_file story_file config.output_path )
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
