open Base
open Stdio

let run ?(quiet = false) verbose path =
  if not quiet then printf "Checking project at: %s\n" path;
  if verbose && not quiet then printf "[verbose] Running validation checks...\n";

  (* Simple validation checks *)
  let checks =
    [
      ("Project directory exists", Stdlib.Sys.file_exists path);
      ("Scenes directory", Stdlib.Sys.file_exists (path ^ "/scenes"));
      ("Config file", Stdlib.Sys.file_exists (path ^ "/narratoric.json"));
    ]
  in

  let all_passed = ref true in

  List.iter checks ~f:(fun (name, passed) ->
      let status = if passed then "✓" else "✗" in
      if not passed then all_passed := false;
      if not quiet then printf "%s %s\n" status name );

  if !all_passed then begin
    if not quiet then printf "\nAll checks passed!\n";
    `Ok 0
  end
  else begin
    if not quiet then printf "\nSome checks failed. Please fix the issues above.\n";
    `Ok 1
  end
