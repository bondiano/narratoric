(** Expect tests for JavaScript code generation *)

open Base
open Stdio
module Lexer = Narratoric.Story.Lexer
module Parser = Narratoric.Story.Parser
module Codegen = Narratoric.Story.Codegen
module Ast = Narratoric.Story.Ast

(** Helper to parse and generate JS *)
let generate_js_normalized story_code ~story_id =
  let tokens = Lexer.lex_story story_code in
  match Parser.parse tokens with
  | Error e -> Printf.sprintf "Parse error: %s" e.message
  | Ok story -> Codegen.compile_to_js ~story_id story

(** Helper to pass through JS string *)
let generate_js_normalized_from_js js = js

(** Print a complete valid JavaScript story object for focused testing *)
let print_story_object story_code =
  let tokens = Lexer.lex_story story_code in
  match Parser.parse tokens with
  | Error e -> printf "Parse error: %s\n" e.message
  | Ok _ ->
      let js = generate_js_normalized story_code ~story_id:"test" in
      print_endline js

let%expect_test "simple_narration" =
  let story = {|## start
This is a simple story.
It has multiple lines.|} in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export const story = {
      states: {
        start: {
          name: "start",
          blocks: [
            {
              type: "narration",
              content: "This is a simple story."
            },
            {
              type: "narration",
              content: "It has multiple lines."
            }
          ]
        }
      },
      runtime
    };
    |}]

let%expect_test "dialogue" =
  let story =
    {|## tavern
Bartender: "Welcome! What can I get you?"
Player: "Just water, thanks."|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export const story = {
      states: {
        tavern: {
          name: "tavern",
          blocks: [
            {
              type: "dialogue",
              speaker: "Bartender",
              content: ""Welcome! What can I get you?""
            },
            {
              type: "dialogue",
              speaker: "Player",
              content: ""Just water, thanks.""
            }
          ]
        }
      },
      runtime
    };
    |}]

let%expect_test "choices" =
  let story =
    {|## crossroads
You reach a crossroads.

* [Go left] -> left_path
* [Go right] -> right_path
* [Turn back]|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export const story = {
      states: {
        crossroads: {
          name: "crossroads",
          blocks: [
            {
              type: "narration",
              content: "You reach a crossroads."
            },
            {
              target: "left_path",
              type: "choice",
              text: "Go left"
            },
            {
              target: "right_path",
              type: "choice",
              text: "Go right"
            },
            {
              type: "choice",
              text: "Turn back"
            }
          ]
        }
      },
      runtime
    };
    |}]

let%expect_test "variables_and_items" =
  let story = {|## shop
$gold = 100
$health = 10
+sword
-potion|} in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export const story = {
      states: {
        shop: {
          name: "shop",
          blocks: [
            {
              type: "variableSet",
              name: "gold",
              value: "100"
            },
            {
              type: "variableSet",
              name: "health",
              value: "10"
            },
            {
              type: "itemAdd",
              item: "sword"
            },
            {
              type: "itemRemove",
              item: "potion"
            }
          ]
        }
      },
      runtime
    };
    |}]

let%expect_test "conditionals" =
  let story =
    {|## treasury
[if $gold >= 50]
  You feel wealthy!
[else]
  You need more gold.
[end]|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export const story = {
      states: {
        treasury: {
          name: "treasury",
          blocks: [
            {
              type: "conditional",
              condition: {
                type: "binary",
                operator: ">=",
                left: {
                  type: "variable",
                  name: "$gold"
                },
                right: 50
              },
              thenBlocks: [
                {
                  type: "narration",
                  content: "You feel wealthy!"
                },
                {
                  type: "conditional",
                  condition: "else",
                  thenBlocks: [
                    {
                      type: "narration",
                      content: "You need more gold."
                    },
                    {
                      type: "conditional",
                      condition: "end",
                      thenBlocks: []
                    }
                  ]
                }
              ]
            }
          ]
        }
      },
      runtime
    };
    |}]

let%expect_test "complex_conditionals" =
  let story =
    {|## complex_check
[if $gold >= 100 and (@player/stats.stamina == 100 or @player/stats.agility < 10) and @my_game/bonus_system.bonuses > 1000]
  You have achieved greatness!
[else]
  Keep working towards your goals.
[end]|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export const story = {
      states: {
        complex_check: {
          name: "complex_check",
          blocks: [
            {
              type: "conditional",
              condition: {
                type: "binary",
                operator: "and",
                left: {
                  type: "binary",
                  operator: "and",
                  left: {
                    type: "binary",
                    operator: "and",
                    left: {
                      type: "binary",
                      operator: ">=",
                      left: {
                        type: "variable",
                        name: "$gold"
                      },
                      right: 100
                    },
                    right: {
                      type: "binary",
                      operator: ">=",
                      left: {
                        type: "variable",
                        name: "$gold"
                      },
                      right: 100
                    }
                  },
                  right: "@player/stats.stamina == 100 or @player/stats.agility < 10"
                },
                right: {
                  type: "binary",
                  operator: ">",
                  left: {
                    type: "variable",
                    name: "@my_game/bonus_system.bonuses"
                  },
                  right: 1000
                }
              },
              thenBlocks: [
                {
                  type: "narration",
                  content: "You have achieved greatness!"
                },
                {
                  type: "conditional",
                  condition: "else",
                  thenBlocks: [
                    {
                      type: "narration",
                      content: "Keep working towards your goals."
                    },
                    {
                      type: "conditional",
                      condition: "end",
                      thenBlocks: []
                    }
                  ]
                }
              ]
            }
          ]
        }
      },
      runtime
    };
    |}]

let%expect_test "skill_checks" =
  let story =
    {|## dungeon
? perception check DC 15
  => You notice a trap!
  =| You see nothing unusual.|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export const story = {
      states: {
        dungeon: {
          name: "dungeon",
          blocks: [
            {
              type: "skillCheck",
              description: "perception check DC 15",
              successBlocks: [
                {
                  type: "narration",
                  content: "You notice a trap!"
                }
              ],
              failureBlocks: [
                {
                  type: "narration",
                  content: "You see nothing unusual."
                }
              ]
            }
          ]
        }
      },
      runtime
    };
    |}]

let%expect_test "directives_and_transitions" =
  let story = {|## cutscene
@play_sound thunder.mp3
The storm begins!
-> next_scene|} in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export const story = {
      states: {
        cutscene: {
          name: "cutscene",
          blocks: [
            {
              type: "directive",
              command: "play_sound",
              params: "thunder.mp3"
            },
            {
              type: "narration",
              content: "The storm begins!"
            },
            {
              type: "transition",
              target: "next_scene"
            }
          ]
        }
      },
      runtime
    };
    |}]

let%expect_test "multiple_states" =
  let story = {|## start
Welcome!
-> main

## main
Main area.

## end
Game over.|} in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export const story = {
      states: {
        start: {
          name: "start",
          blocks: [
            {
              type: "narration",
              content: "Welcome!"
            },
            {
              type: "transition",
              target: "main"
            }
          ]
        },
        main: {
          name: "main",
          blocks: [
            {
              type: "narration",
              content: "Main area."
            }
          ]
        },
        end: {
          name: "end",
          blocks: [
            {
              type: "narration",
              content: "Game over."
            }
          ]
        }
      },
      runtime
    };
    |}]

let%expect_test "escaping" =
  let story = {|## test
Text with "quotes" and \backslash.|} in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export const story = {
      states: {
        test: {
          name: "test",
          blocks: [
            {
              type: "narration",
              content: "Text with "quotes" and \backslash."
            }
          ]
        }
      },
      runtime
    };
    |}]

let%expect_test "manual_ast_conditional" =
  (* Manually create AST to test codegen *)
  let condition_ast =
    Ast.BinaryOp
      { op = Ast.Gte; left = Ast.Variable "$gold"; right = Ast.Literal (Ast.Int 50) }
  in
  let then_blocks = [ Ast.Narration "You feel wealthy!" ] in
  let else_blocks = [ Ast.Narration "You need more gold." ] in
  let conditional_block =
    Ast.Conditional
      { condition = condition_ast; then_blocks; else_blocks = Some else_blocks }
  in
  let state = { Ast.name = "treasury"; blocks = [ conditional_block ] } in
  let story = [ state ] in
  let js = Codegen.compile_to_js ~story_id:"test" story in
  let normalized = generate_js_normalized_from_js js in
  print_endline normalized;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export const story = {
      states: {
        treasury: {
          name: "treasury",
          blocks: [
            {
              elseBlocks: [
                {
                  type: "narration",
                  content: "You need more gold."
                }
              ],
              type: "conditional",
              condition: {
                type: "binary",
                operator: ">=",
                left: {
                  type: "variable",
                  name: "$gold"
                },
                right: 50
              },
              thenBlocks: [
                {
                  type: "narration",
                  content: "You feel wealthy!"
                }
              ]
            }
          ]
        }
      },
      runtime
    };
    |}]
