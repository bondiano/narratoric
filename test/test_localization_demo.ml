open Base
open Stdio
open Narratoric.Story

let test_content =
  {|@scene
@background tavern_interior

## start
%{tavern.entrance.welcome}
Welcome to the Drunken Griffin!

Bartender: %{npc.bartender.greeting}
"What can I get you, traveler?"

* %{choice.order_drink} [Order a drink] -> drink
* [Ask about rumors] -> rumors
|}

let () =
  printf "=== Testing Localization System ===\n\n";

  (* Test lexer *)
  printf "1. Lexing tokens:\n";
  let tokens = Lexer.lex_story test_content in
  List.iter tokens ~f:(function
    | LocaleKey key -> printf "  - LocaleKey: %s\n" key
    | Text text -> printf "  - Text: %s\n" text
    | _ -> () );

  (* Test parser *)
  printf "\n2. Parsing AST:\n";
  match Parser.parse ~filename:"test.story" tokens with
  | Error e -> printf "Parse error: %s\n" e.message
  | Ok story ->
      printf "✓ Parsed successfully\n";
      printf "Story type: %s\n"
        ( match story.metadata.story_type with
        | Ast.Scene _ -> "scene"
        | Ast.Npc _ -> "npc"
        | Ast.Merchant _ -> "merchant"
        | Ast.Quest _ -> "quest" );

      (* Test codegen *)
      printf "\n3. Generated JavaScript:\n";
      let js = Codegen.compile_to_js ~story_id:"test" story in
      printf "%s\n" js
