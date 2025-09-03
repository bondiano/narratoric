open Base
open OUnit2

let test_check_command _ =
  let result = Narrat_cli.Check.run false "." in
  assert_bool "Check command should run successfully"
    (match result with `Ok _ -> true | _ -> false);

  let result = Narrat_cli.Check.run false "/non/existent/path" in
  assert_bool "Check command should handle missing directory"
    (match result with `Ok 1 -> true | _ -> false)

let test_build_config ctxt =
  let config : Narrat_cli.Build.config =
    {
      source_path = "./scenes";
      output_path = "./dist";
      watch_mode = false;
      verbose = false;
    }
  in

  assert_equal ~ctxt ~printer:(fun x -> x) "./scenes" config.source_path;
  assert_equal ~ctxt ~printer:(fun x -> x) "./dist" config.output_path;
  assert_bool "watch_mode should be false" (not config.watch_mode)

let test_run_config ctxt =
  let config : Narrat_cli.Run.config =
    { project_path = "."; port = 3000; dev_mode = false; verbose = false }
  in

  assert_equal ~ctxt ~printer:Int.to_string 3000 config.port;
  assert_equal ~ctxt ~printer:(fun x -> x) "." config.project_path;
  assert_bool "dev_mode should be false" (not config.dev_mode)

let test_init_templates _ =
  (* Test that templates are available *)
  let templates = Narrat_cli.Init.available_templates in
  assert_bool "Templates list should not be empty" (not (List.is_empty templates));

  (* Check basic template exists *)
  let has_basic = List.exists templates ~f:(fun (name, _) -> String.equal name "basic") in
  assert_bool "Basic template should exist" has_basic;

  (* Check visual-novel template exists *)
  let has_vn =
    List.exists templates ~f:(fun (name, _) -> String.equal name "visual-novel")
  in
  assert_bool "Visual novel template should exist" has_vn

let test_test_suite_runner _ =
  (* Test result structure *)
  let result : Narrat_cli.Test.test_result =
    { name = "Sample test"; passed = true; duration_ms = 1.5; error = None }
  in

  assert_bool "Test should pass" result.passed;
  assert_bool "Duration should be positive" Float.(result.duration_ms > 0.0);
  assert_bool "Error should be None" (Option.is_none result.error)

let test_cli_version ctxt =
  let version = Narrat_cli.Cli.version in
  assert_equal ~ctxt ~printer:(fun x -> x) "0.1.0" version

let test_main_command ctxt =
  let main_cmd = Narrat_cli.Cli.main_cmd in
  assert_equal ~ctxt ~printer:(fun x -> x) "narratoric" (Cmdliner.Cmd.name main_cmd)

let suite =
  "CLI Test Suite"
  >::: [
         "test_cli_version" >:: test_cli_version;
         "test_main_command" >:: test_main_command;
         "test_check_command" >:: test_check_command;
         "test_build_config" >:: test_build_config;
         "test_run_config" >:: test_run_config;
         "test_init_templates" >:: test_init_templates;
         "test_test_suite_runner" >:: test_test_suite_runner;
       ]

let () = run_test_tt_main suite
