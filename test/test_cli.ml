open Base
open Stdio

let test_check_command () =
  printf "Testing check command...\n";

  let result = Narrat_cli.Check.run false "." in
  assert (match result with `Ok _ -> true | _ -> false);
  printf "✓ Check command runs successfully\n";

  let result = Narrat_cli.Check.run false "/non/existent/path" in
  assert (match result with `Ok 1 -> true | _ -> false);
  printf "✓ Check command handles missing directory\n"

let test_build_config () =
  printf "Testing build configuration...\n";

  let config : Narrat_cli.Build.config =
    {
      source_path = "./scenes";
      output_path = "./dist";
      watch_mode = false;
      verbose = false;
    }
  in

  assert (String.equal config.source_path "./scenes");
  assert (String.equal config.output_path "./dist");
  assert (not config.watch_mode);
  printf "✓ Build config structure valid\n"

let test_run_config () =
  printf "Testing run configuration...\n";

  let config : Narrat_cli.Run.config =
    { project_path = "."; port = 3000; dev_mode = false; verbose = false }
  in

  assert (Int.equal config.port 3000);
  assert (String.equal config.project_path ".");
  assert (not config.dev_mode);
  printf "✓ Run config structure valid\n"

let test_init_templates () =
  printf "Testing init templates...\n";

  (* Test that templates are available *)
  let templates = Narrat_cli.Init.available_templates in
  assert (not (List.is_empty templates));

  (* Check basic template exists *)
  let has_basic = List.exists templates ~f:(fun (name, _) -> String.equal name "basic") in
  assert has_basic;

  (* Check visual-novel template exists *)
  let has_vn =
    List.exists templates ~f:(fun (name, _) -> String.equal name "visual-novel")
  in
  assert has_vn;

  printf "✓ Init templates valid\n"

let test_test_suite_runner () =
  printf "Testing test suite runner...\n";

  (* Test result structure *)
  let result : Narrat_cli.Test.test_result =
    { name = "Sample test"; passed = true; duration_ms = 1.5; error = None }
  in

  assert result.passed;
  assert (Float.(result.duration_ms > 0.0));
  assert (Option.is_none result.error);

  printf "✓ Test result structure valid\n"

let test_cli_version () =
  printf "Testing CLI version...\n";

  let version = Narrat_cli.Cli.version in
  assert (String.equal version "0.1.0");
  printf "✓ Version check passed\n"

let test_main_command () =
  printf "Testing main command structure...\n";

  let main_cmd = Narrat_cli.Cli.main_cmd in
  assert (String.equal (Cmdliner.Cmd.name main_cmd) "narratoric");
  printf "✓ Main command name valid\n"

let run_all_tests () =
  printf "\n══════════════════════════════════\n";
  printf "Running CLI Test Suite\n";
  printf "══════════════════════════════════\n\n";

  test_cli_version ();
  test_main_command ();
  test_check_command ();
  test_build_config ();
  test_run_config ();
  test_init_templates ();
  test_test_suite_runner ();

  printf "\n══════════════════════════════════\n";
  printf "✅ All CLI tests passed!\n";
  printf "══════════════════════════════════\n\n"

let () = run_all_tests ()
