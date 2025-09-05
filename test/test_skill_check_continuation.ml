(** Test for skill check continuation and transitions *)

open Base
open Stdio
module Ast = Narratoric.Story.Ast
module Lexer = Narratoric.Story.Lexer
module Parser = Narratoric.Story.Parser
module Codegen = Narratoric.Story.Codegen

(** Helper to parse and generate JS *)
let generate_js_normalized story_code ~story_id =
  let tokens = Lexer.lex_story story_code in
  match Parser.parse ~filename:"test.story" tokens with
  | Error e -> Printf.sprintf "Parse error: %s" e.Parser.message
  | Ok story -> Codegen.compile_to_js ~story_id story

(** Print a complete valid JavaScript story object for focused testing *)
let print_story_object story_code =
  let tokens = Lexer.lex_story story_code in
  match Parser.parse ~filename:"test.story" tokens with
  | Error e -> printf "Parse error: %s\n" e.Parser.message
  | Ok _ ->
      let js = generate_js_normalized story_code ~story_id:"test" in
      print_endline js

let%expect_test "skill_check_with_continuation" =
  let story =
    {|@scene

## tavern
Bartender: "There's something strange about that corner..."

? perception DC 15
  => You notice a hidden door behind the tapestry!
  =| You see nothing unusual.

The bartender continues cleaning glasses.

* [Ask about the corner] -> investigate
* [Order a drink] -> order
-> next_state

## next_state
You continue on.|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export default {
      type: "scene",
      states: {
        tavern: {
          name: "tavern",
          blocks: [
            {
              type: "dialogue",
              speaker: "Bartender",
              content: {
                text: ""There's something strange about that corner...""
              }
            },
            {
              type: "skillCheck",
              skillType: "perception",
              difficulty: 15,
              description: {
                text: "perception DC 15"
              },
              successBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "You notice a hidden door behind the tapestry!"
                  }
                }
              ],
              failureBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "You see nothing unusual."
                  }
                },
                {
                  type: "narration",
                  content: {
                    text: "The bartender continues cleaning glasses."
                  }
                }
              ]
            },
            {
              target: "investigate",
              type: "choice",
              text: {
                text: "Ask about the corner"
              }
            },
            {
              target: "order",
              type: "choice",
              text: {
                text: "Order a drink"
              }
            },
            {
              type: "transition",
              target: "next_state"
            }
          ]
        },
        next_state: {
          name: "next_state",
          blocks: [
            {
              type: "narration",
              content: {
                text: "You continue on."
              }
            }
          ]
        }
      },
      scene: {},
      runtime
    };
    |}]

let%expect_test "skill_check_with_transitions_in_outcomes" =
  let story =
    {|@scene

## room
You enter a dark room.

? perception DC 12
  => You spot a trap! You carefully avoid it.
     -> safe_passage
  =| You stumble forward blindly.
     -> trapped

## safe_passage
You made it through safely!

## trapped
You triggered a trap!|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export default {
      type: "scene",
      states: {
        room: {
          name: "room",
          blocks: [
            {
              type: "narration",
              content: {
                text: "You enter a dark room."
              }
            },
            {
              type: "skillCheck",
              skillType: "perception",
              difficulty: 12,
              description: {
                text: "perception DC 12"
              },
              successBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "You spot a trap! You carefully avoid it."
                  }
                },
                {
                  type: "transition",
                  target: "safe_passage"
                }
              ],
              failureBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "You stumble forward blindly."
                  }
                },
                {
                  type: "transition",
                  target: "trapped"
                }
              ]
            }
          ]
        },
        safe_passage: {
          name: "safe_passage",
          blocks: [
            {
              type: "narration",
              content: {
                text: "You made it through safely!"
              }
            }
          ]
        },
        trapped: {
          name: "trapped",
          blocks: [
            {
              type: "narration",
              content: {
                text: "You triggered a trap!"
              }
            }
          ]
        }
      },
      scene: {},
      runtime
    };
    |}]

let%expect_test "skill_check_mixed_with_other_blocks" =
  let story =
    {|@scene

## dungeon
The corridor is dark and damp.

$torches = 3

? stealth DC 14
  => You move silently through the shadows.
  =| Your footsteps echo loudly!

+map

The passage continues ahead.

@play_sound ambient_dungeon.mp3

-> continue

## continue
You press on.|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export default {
      type: "scene",
      states: {
        dungeon: {
          name: "dungeon",
          blocks: [
            {
              type: "narration",
              content: {
                text: "The corridor is dark and damp."
              }
            },
            {
              type: "variableSet",
              name: "torches",
              value: "3"
            },
            {
              type: "skillCheck",
              skillType: "stealth",
              difficulty: 14,
              description: {
                text: "stealth DC 14"
              },
              successBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "You move silently through the shadows."
                  }
                }
              ],
              failureBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "Your footsteps echo loudly!"
                  }
                }
              ]
            },
            {
              type: "itemAdd",
              item: "map"
            },
            {
              type: "narration",
              content: {
                text: "The passage continues ahead."
              }
            },
            {
              type: "directive",
              command: "play_sound",
              params: "ambient_dungeon.mp3"
            },
            {
              type: "transition",
              target: "continue"
            }
          ]
        },
        continue: {
          name: "continue",
          blocks: [
            {
              type: "narration",
              content: {
                text: "You press on."
              }
            }
          ]
        }
      },
      scene: {},
      runtime
    };
    |}]