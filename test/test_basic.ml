open Base
open Stdio

let test_engine_creation () =
  let engine = Narratoric.Engine.create "TestPlayer" in
  assert (String.equal (Narratoric.Engine.get_player_name engine) "TestPlayer");
  assert (Option.is_none (Narratoric.Engine.get_current_scene engine))

let test_variable_operations () =
  let engine = Narratoric.Engine.create "Player" in

  let engine =
    Narratoric.Engine.process_event engine
      (Narratoric.Events.VariableSet ("health", "100"))
  in

  match Narratoric.Engine.get_variable engine "health" with
  | Some value -> assert (String.equal value "100")
  | None -> assert false

let test_scene_operations () =
  let engine = Narratoric.Engine.create "Player" in
  let engine =
    Narratoric.Engine.process_event engine (Narratoric.Events.SceneChange "intro")
  in

  match Narratoric.Engine.get_current_scene engine with
  | Some scene -> assert (String.equal scene "intro")
  | None -> assert false

let test_event_processing () =
  let engine = Narratoric.Engine.create "Player" in
  let engine = Narratoric.Engine.process_event engine Narratoric.Events.GameStart in
  let engine =
    Narratoric.Engine.process_event engine (Narratoric.Events.SceneChange "main")
  in
  let engine =
    Narratoric.Engine.process_event engine (Narratoric.Events.VariableSet ("score", "50"))
  in

  match
    ( Narratoric.Engine.get_current_scene engine,
      Narratoric.Engine.get_variable engine "score" )
  with
  | Some scene, Some score ->
      assert (String.equal scene "main");
      assert (String.equal score "50")
  | _ -> assert false

let () =
  printf "Running game engine tests...\n";

  test_engine_creation ();
  printf "✓ Engine creation test passed\n";

  test_variable_operations ();
  printf "✓ Variable operations test passed\n";

  test_scene_operations ();
  printf "✓ Scene operations test passed\n";

  test_event_processing ();
  printf "✓ Event processing test passed\n";

  printf "\nAll tests passed! ✅\n"
