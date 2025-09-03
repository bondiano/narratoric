open Base
open Stdio

type config = {
  project_path : string;
  port : int;
  dev_mode : bool;
  verbose : bool;
}

let start_server config =
  printf "Starting narrative engine...\n";
  printf "Project: %s\n" config.project_path;
  printf "Port: %d\n" config.port;

  if config.dev_mode then printf "Running in development mode with hot reload\n"
  else printf "Running in production mode\n";

  if config.verbose then printf "[verbose] Starting server...\n";

  (* TODO: Engine implementation is in Phase 3 *)
  printf "\nNarrative Engine Server Started\n";
  printf "Note: Engine runtime will be implemented in Phase 3\n";

  printf "\n═══════════════════════════════════════\n";
  printf "  Server running at http://localhost:%d\n" config.port;
  printf "  Press Ctrl+C to stop\n";
  printf "═══════════════════════════════════════\n\n";

  (* TODO: Implement actual HTTP server *)
  (* For now, just block *)
  if config.dev_mode then begin
    printf "[dev] Watching for file changes...\n"
  end;

  (* Simple blocking loop for demo *)
  let rec wait () =
    Unix.sleep 1;
    wait ()
  in

  try wait ()
  with _ ->
    printf "\nShutting down server...\n";
    `Ok 0

let run config = start_server config
