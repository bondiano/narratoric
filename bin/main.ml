open Base
open Stdio

let () =
  let engine = Narratoric.Engine.create "Player" in

  printf "Narrative Engine Demo\n";
  printf "Player: %s\n" (Narratoric.Engine.get_player_name engine);

  let engine =
    Narratoric.Engine.process_event engine (Narratoric.Events.SceneChange "intro")
  in
  let engine =
    Narratoric.Engine.process_event engine
      (Narratoric.Events.VariableSet ("health", "100"))
  in

  printf "Current scene: %s\n"
    (Option.value (Narratoric.Engine.get_current_scene engine) ~default:"none");
  printf "Health: %s\n"
    (Option.value (Narratoric.Engine.get_variable engine "health") ~default:"unknown");

  printf "Game engine initialized successfully!\n"
