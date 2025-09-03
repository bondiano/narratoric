open Cmdliner

let version = "0.1.0"

let verbose =
  let doc = "Enable verbose output" in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

let project_path =
  let doc = "Path to the project directory" in
  Arg.(value & opt string "." & info [ "p"; "path" ] ~docv:"PATH" ~doc)

let check_cmd =
  let doc = "Check project configuration and dependencies" in
  let man =
    [
      `S Manpage.s_description;
      `P "Validates the project structure and configuration files.";
      `P "Checks for required directories, configuration files, and dependencies.";
    ]
  in
  let cmd verbose path =
    match Check.run verbose path with `Ok code -> Stdlib.exit code | _ -> ()
  in
  let term = Term.(const cmd $ verbose $ project_path) in
  let info = Cmd.info "check" ~doc ~man in
  Cmd.v info term

let build_cmd =
  let doc = "Build the project" in
  let man =
    [
      `S Manpage.s_description;
      `P "Compiles the narrative project into a runnable format.";
    ]
  in
  let output =
    let doc = "Output directory for built files" in
    Arg.(value & opt string "./build" & info [ "o"; "output" ] ~docv:"DIR" ~doc)
  in
  let watch =
    let doc = "Watch for file changes and rebuild automatically" in
    Arg.(value & flag & info [ "w"; "watch" ] ~doc)
  in
  let cmd verbose path output watch =
    let config =
      Build.{ source_path = path; output_path = output; watch_mode = watch; verbose }
    in
    match Build.run config with `Ok code -> Stdlib.exit code | _ -> ()
  in
  let term = Term.(const cmd $ verbose $ project_path $ output $ watch) in
  let info = Cmd.info "build" ~doc ~man in
  Cmd.v info term

let run_cmd =
  let doc = "Run the narrative game" in
  let man =
    [
      `S Manpage.s_description;
      `P "Starts the narrative game engine with the specified project.";
      `P "Launches a local server for development or production use.";
    ]
  in
  let port =
    let doc = "Port for the development server" in
    Arg.(value & opt int 3000 & info [ "port" ] ~docv:"PORT" ~doc)
  in
  let dev =
    let doc = "Run in development mode with hot reload" in
    Arg.(value & flag & info [ "d"; "dev" ] ~doc)
  in
  let cmd verbose path port dev =
    let config = Run.{ project_path = path; port; dev_mode = dev; verbose } in
    match Run.run config with `Ok code -> Stdlib.exit code | _ -> ()
  in
  let term = Term.(const cmd $ verbose $ project_path $ port $ dev) in
  let info = Cmd.info "run" ~doc ~man in
  Cmd.v info term

(* Init command *)
let init_cmd =
  let doc = "Initialize a new narrative project" in
  let man =
    [
      `S Manpage.s_description;
      `P "Creates a new narrative project with default structure and configuration.";
      `P "Available templates:";
      `P "  - basic: Simple narrative game structure";
      `P "  - visual-novel: Visual novel with sprites and backgrounds";
      `P "  - interactive-fiction: Text-based adventure";
      `P "  - custom: Minimal structure for custom projects";
    ]
  in
  let template =
    let doc = "Project template to use" in
    let templates = [ "basic"; "visual-novel"; "interactive-fiction"; "custom" ] in
    Arg.(
      value
      & opt (enum (List.map (fun t -> (t, t)) templates)) "basic"
      & info [ "t"; "template" ] ~docv:"TEMPLATE" ~doc )
  in
  let project_name =
    let doc = "Project name" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let cmd verbose project_name template =
    match Init.run project_name template verbose "." with
    | `Ok code -> Stdlib.exit code
    | _ -> ()
  in
  let term = Term.(const cmd $ verbose $ project_name $ template) in
  let info = Cmd.info "init" ~doc ~man in
  Cmd.v info term

let test_cmd =
  let doc = "Run project tests" in
  let man =
    [
      `S Manpage.s_description;
      `P "Executes test suites for the narrative project.";
      `P "Runs unit tests, integration tests, and scene validation.";
    ]
  in
  let coverage =
    let doc = "Generate code coverage report" in
    Arg.(value & flag & info [ "c"; "coverage" ] ~doc)
  in
  let filter =
    let doc = "Filter tests by pattern" in
    Arg.(value & opt (some string) None & info [ "f"; "filter" ] ~docv:"PATTERN" ~doc)
  in
  let cmd verbose path coverage filter =
    match Test.run path coverage filter verbose with
    | `Ok code -> Stdlib.exit code
    | _ -> ()
  in
  let term = Term.(const cmd $ verbose $ project_path $ coverage $ filter) in
  let info = Cmd.info "test" ~doc ~man in
  Cmd.v info term

let main_cmd =
  let doc = "Narrative Engine CLI" in
  let man =
    [
      `S Manpage.s_description;
      `P "Command-line interface for managing and running narrative games.";
      `P "Create, build, and run interactive narrative experiences.";
      `P "Use $(mname) $(i,COMMAND) --help for more information on a command.";
      `S Manpage.s_examples;
      `P "Create a new project:";
      `Pre "  \\$ narratoric init my-game --template visual-novel";
      `P "Check project configuration:";
      `Pre "  \\$ narratoric check --path ./my-game";
      `P "Build the project:";
      `Pre "  \\$ narratoric build --watch";
      `P "Run in development mode:";
      `Pre "  \\$ narratoric run --dev --port 8080";
      `S Manpage.s_bugs;
      `P "Report bugs at https://github.com/bondiano/narratoric/issues";
    ]
  in
  let info = Cmd.info "narratoric" ~version ~doc ~man in

  Cmd.group info [ check_cmd; build_cmd; run_cmd; init_cmd; test_cmd ]

(* Entry point *)
let run () = Stdlib.exit (Cmd.eval main_cmd)
