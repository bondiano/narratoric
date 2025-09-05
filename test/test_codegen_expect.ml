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
  match Parser.parse ~filename:"test.story" tokens with
  | Error e -> Printf.sprintf "Parse error: %s" e.message
  | Ok story -> Codegen.compile_to_js ~story_id story

(** Helper to pass through JS string *)
let generate_js_normalized_from_js js = js

(** Print a complete valid JavaScript story object for focused testing *)
let print_story_object story_code =
  let tokens = Lexer.lex_story story_code in
  match Parser.parse ~filename:"test.story" tokens with
  | Error e -> printf "Parse error: %s\n" e.message
  | Ok _ ->
      let js = generate_js_normalized story_code ~story_id:"test" in
      print_endline js

let%expect_test "missing_story_type_error" =
  let story = {|## start
This is a simple story without a type.|} in
  print_story_object story;
  [%expect
    {| Parse error: Story type not specified in file 'test.story'. Please add @scene, @npc, @merchant, or @quest at the beginning of the file (line 1, column 1) |}]

let%expect_test "simple_narration" =
  let story = {|@scene

## start
This is a simple story.
It has multiple lines.|} in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export default {
      type: "scene",
      states: {
        start: {
          name: "start",
          blocks: [
            {
              type: "narration",
              content: {
                text: "This is a simple story."
              }
            },
            {
              type: "narration",
              content: {
                text: "It has multiple lines."
              }
            }
          ]
        }
      },
      scene: {},
      runtime
    };
    |}]

let%expect_test "scene_with_background_music" =
  let story =
    {|@scene
@background tavern_interior
@music ambient_tavern.mp3

## tavern
The tavern is warm and inviting.|}
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
              type: "narration",
              content: {
                text: "The tavern is warm and inviting."
              }
            }
          ]
        }
      },
      scene: {
        background: "tavern_interior",
        music: "ambient_tavern.mp3"
      },
      runtime
    };
    |}]

let%expect_test "npc_dialogue" =
  let story =
    {|@npc bartender

## greeting
Bartender: "Welcome! What can I get you?"
Player: "Just water, thanks."

## offer_quest
Bartender: "You look like someone who could help with a problem..."|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export default {
      type: "npc",
      states: {
        greeting: {
          name: "greeting",
          blocks: [
            {
              type: "dialogue",
              speaker: "Bartender",
              content: {
                text: ""Welcome! What can I get you?""
              }
            },
            {
              type: "dialogue",
              speaker: "Player",
              content: {
                text: ""Just water, thanks.""
              }
            }
          ]
        },
        offer_quest: {
          name: "offer_quest",
          blocks: [
            {
              type: "dialogue",
              speaker: "Bartender",
              content: {
                text: ""You look like someone who could help with a problem...""
              }
            }
          ]
        }
      },
      npc: {
        name: {
          text: "bartender"
        }
      },
      runtime
    };
    |}]

let%expect_test "merchant_with_inventory" =
  let story =
    {|@merchant blacksmith

## greeting
Blacksmith: "Looking for weapons or armor?"

* [Show me weapons] -> weapons
* [Show me armor] -> armor
* [Leave] -> exit

## weapons
+sword
+dagger
+bow

## armor
+chainmail
+helmet
+shield|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export default {
      type: "merchant",
      states: {
        greeting: {
          name: "greeting",
          blocks: [
            {
              type: "dialogue",
              speaker: "Blacksmith",
              content: {
                text: ""Looking for weapons or armor?""
              }
            },
            {
              target: "weapons",
              type: "choice",
              text: {
                text: "Show me weapons"
              }
            },
            {
              target: "armor",
              type: "choice",
              text: {
                text: "Show me armor"
              }
            },
            {
              target: "exit",
              type: "choice",
              text: {
                text: "Leave"
              }
            }
          ]
        },
        weapons: {
          name: "weapons",
          blocks: [
            {
              type: "itemAdd",
              item: "sword"
            },
            {
              type: "itemAdd",
              item: "dagger"
            },
            {
              type: "itemAdd",
              item: "bow"
            }
          ]
        },
        armor: {
          name: "armor",
          blocks: [
            {
              type: "itemAdd",
              item: "chainmail"
            },
            {
              type: "itemAdd",
              item: "helmet"
            },
            {
              type: "itemAdd",
              item: "shield"
            }
          ]
        }
      },
      merchant: {
        name: {
          text: "blacksmith"
        }
      },
      runtime
    };
    |}]

let%expect_test "merchant_with_initial_gold" =
  let story =
    {|@merchant shopkeeper
@initial_gold 500

## greeting
Shopkeeper: "Welcome to my shop! I have the finest wares in the land."

## shop
* [Buy health potion (50 gold)] -> buy_potion
* [Sell rusty sword (20 gold)] -> sell_sword
* [Leave] -> exit|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export default {
      type: "merchant",
      states: {
        greeting: {
          name: "greeting",
          blocks: [
            {
              type: "dialogue",
              speaker: "Shopkeeper",
              content: {
                text: ""Welcome to my shop! I have the finest wares in the land.""
              }
            }
          ]
        },
        shop: {
          name: "shop",
          blocks: [
            {
              target: "buy_potion",
              type: "choice",
              text: {
                text: "Buy health potion (50 gold)"
              }
            },
            {
              target: "sell_sword",
              type: "choice",
              text: {
                text: "Sell rusty sword (20 gold)"
              }
            },
            {
              target: "exit",
              type: "choice",
              text: {
                text: "Leave"
              }
            }
          ]
        }
      },
      merchant: {
        name: {
          text: "shopkeeper"
        },
        initialGold: 500
      },
      runtime
    };
    |}]

let%expect_test "quest_story" =
  let story =
    {|@quest Find the Lost Artifact
@description An ancient artifact has been stolen from the museum. Help recover it!
@objective Find clues about the thief
@objective Track down the thief's hideout
@objective Recover the artifact
@success_description You've successfully recovered the artifact and saved the day!
@failed_description The artifact was lost forever...

## start
Museum Curator: "Thank you for coming! The artifact was stolen last night."

* [Examine the crime scene] -> investigate
* [Question witnesses] -> witnesses
* [Check security footage] -> footage

## investigate
You find muddy footprints leading to the window.
$clue_footprints = true
-> continue_investigation

## witnesses
Guard: "I saw someone in a black cloak around midnight."
$clue_blackcloak = true
-> continue_investigation

## continue_investigation
[if $clue_footprints and $clue_blackcloak]
  You have enough clues to track the thief!
  -> find_hideout|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export default {
      type: "quest",
      states: {
        start: {
          name: "start",
          blocks: [
            {
              type: "dialogue",
              speaker: "Museum Curator",
              content: {
                text: ""Thank you for coming! The artifact was stolen last night.""
              }
            },
            {
              target: "investigate",
              type: "choice",
              text: {
                text: "Examine the crime scene"
              }
            },
            {
              target: "witnesses",
              type: "choice",
              text: {
                text: "Question witnesses"
              }
            },
            {
              target: "footage",
              type: "choice",
              text: {
                text: "Check security footage"
              }
            }
          ]
        },
        investigate: {
          name: "investigate",
          blocks: [
            {
              type: "narration",
              content: {
                text: "You find muddy footprints leading to the window."
              }
            },
            {
              type: "variableSet",
              name: "clue_footprints",
              value: "true"
            },
            {
              type: "transition",
              target: "continue_investigation"
            }
          ]
        },
        witnesses: {
          name: "witnesses",
          blocks: [
            {
              type: "dialogue",
              speaker: "Guard",
              content: {
                text: ""I saw someone in a black cloak around midnight.""
              }
            },
            {
              type: "variableSet",
              name: "clue_blackcloak",
              value: "true"
            },
            {
              type: "transition",
              target: "continue_investigation"
            }
          ]
        },
        continue_investigation: {
          name: "continue_investigation",
          blocks: [
            {
              type: "conditional",
              condition: {
                type: "binary",
                operator: "and",
                left: {
                  type: "binary",
                  operator: "and",
                  left: "$clue_footprints",
                  right: "$clue_footprints"
                },
                right: "$clue_blackcloak"
              },
              thenBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "You have enough clues to track the thief!"
                  }
                },
                {
                  type: "transition",
                  target: "find_hideout"
                }
              ]
            }
          ]
        }
      },
      quest: {
        title: {
          text: "Find the Lost Artifact"
        },
        description: {
          text: "An ancient artifact has been stolen from the museum. Help recover it!"
        },
        objectives: [
          {
            text: "Find clues about the thief"
          },
          {
            text: "Track down the thief's hideout"
          },
          {
            text: "Recover the artifact"
          }
        ],
        successDescription: {
          text: "You've successfully recovered the artifact and saved the day!"
        },
        failedDescription: {
          text: "The artifact was lost forever..."
        }
      },
      runtime
    };
    |}]

let%expect_test "scene_with_directives" =
  let story =
    {|@scene
@background dungeon_room
@tags interactive, puzzle
@uses inventory, effects

## treasure_room
(* Scene with interactive objects *)

@create_object treasure_chest position:center
@move_object treasure_chest to:{x:600,y:400} duration:2s

You see a mysterious chest in the center of the room.

* [Open the chest] -> open_chest
* [Leave it alone] -> exit

## open_chest
@text_popup "+10 XP" position:player color:gold
@remove_object treasure_chest fade:true

You found a magical amulet!
+amulet|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export default {
      type: "scene",
      states: {
        treasure_room: {
          name: "treasure_room",
          blocks: [
            {
              type: "directive",
              command: "create_object",
              params: "treasure_chest position:center"
            },
            {
              type: "directive",
              command: "move_object",
              params: "treasure_chest to:{x:600,y:400} duration:2s"
            },
            {
              type: "narration",
              content: {
                text: "You see a mysterious chest in the center of the room."
              }
            },
            {
              target: "open_chest",
              type: "choice",
              text: {
                text: "Open the chest"
              }
            },
            {
              target: "exit",
              type: "choice",
              text: {
                text: "Leave it alone"
              }
            }
          ]
        },
        open_chest: {
          name: "open_chest",
          blocks: [
            {
              type: "directive",
              command: "text_popup",
              params: ""+10 XP" position:player color:gold"
            },
            {
              type: "directive",
              command: "remove_object",
              params: "treasure_chest fade:true"
            },
            {
              type: "narration",
              content: {
                text: "You found a magical amulet!"
              }
            },
            {
              type: "itemAdd",
              item: "amulet"
            }
          ]
        }
      },
      scene: {
        background: "dungeon_room"
      },
      tags: [
        "interactive",
        "puzzle"
      ],
      plugins: [
        "inventory",
        "effects"
      ],
      runtime
    };
    |}]

let%expect_test "interactive_zones" =
  let story =
    {|@scene

## hallway
@hotspot door_area {x:100,y:200,w:50,h:100}
  on_click: -> enter_door
  on_hover: @highlight door

You stand in a long hallway with a door at the end.|}
  in
  (* Note: hotspot is currently not a recognized directive format, will be treated as
     directive *)
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export default {
      type: "scene",
      states: {
        hallway: {
          name: "hallway",
          blocks: [
            {
              type: "directive",
              command: "hotspot",
              params: "door_area {x:100,y:200,w:50,h:100}"
            },
            {
              type: "narration",
              content: {
                text: "on_click"
              }
            },
            {
              type: "transition",
              target: "enter_door"
            },
            {
              type: "narration",
              content: {
                text: "on_hover"
              }
            },
            {
              type: "directive",
              command: "highlight",
              params: "door"
            },
            {
              type: "narration",
              content: {
                text: "You stand in a long hallway with a door at the end."
              }
            }
          ]
        }
      },
      scene: {},
      runtime
    };
    |}]

let%expect_test "choices" =
  let story =
    {|@scene

## crossroads
You reach a crossroads.

* [Go left] -> left_path
* [Go right] -> right_path
* [Turn back]|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export default {
      type: "scene",
      states: {
        crossroads: {
          name: "crossroads",
          blocks: [
            {
              type: "narration",
              content: {
                text: "You reach a crossroads."
              }
            },
            {
              target: "left_path",
              type: "choice",
              text: {
                text: "Go left"
              }
            },
            {
              target: "right_path",
              type: "choice",
              text: {
                text: "Go right"
              }
            },
            {
              type: "choice",
              text: {
                text: "Turn back"
              }
            }
          ]
        }
      },
      scene: {},
      runtime
    };
    |}]

let%expect_test "variables_and_items" =
  let story = {|@scene

## shop
$gold = 100
$health = 10
+sword
-potion|} in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export default {
      type: "scene",
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
      scene: {},
      runtime
    };
    |}]

let%expect_test "skill_checks" =
  let story =
    {|@scene

## dungeon
? perception check DC 15
  => You notice a trap!
  =| You see nothing unusual.|}
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
              type: "skillCheck",
              skillType: "perception",
              difficulty: 15,
              description: {
                text: "perception check DC 15"
              },
              successBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "You notice a trap!"
                  }
                }
              ],
              failureBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "You see nothing unusual."
                  }
                }
              ]
            }
          ]
        }
      },
      scene: {},
      runtime
    };
    |}]

let%expect_test "skill_checks_various_formats" =
  let story =
    {|@scene

## tests
? agility DC 20
  => You leap gracefully!
  =| You stumble and fall.

? strength 25
  => You lift the heavy stone!
  =| The stone won't budge.

? lockpicking difficulty 18
  => *Click* The lock opens!
  =| The lock remains secure.|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export default {
      type: "scene",
      states: {
        tests: {
          name: "tests",
          blocks: [
            {
              type: "skillCheck",
              skillType: "agility",
              difficulty: 20,
              description: {
                text: "agility DC 20"
              },
              successBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "You leap gracefully!"
                  }
                }
              ],
              failureBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "You stumble and fall."
                  }
                }
              ]
            },
            {
              type: "skillCheck",
              skillType: "strength",
              difficulty: 25,
              description: {
                text: "strength 25"
              },
              successBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "You lift the heavy stone!"
                  }
                }
              ],
              failureBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "The stone won't budge."
                  }
                }
              ]
            },
            {
              type: "skillCheck",
              skillType: "lockpicking",
              difficulty: 18,
              description: {
                text: "lockpicking difficulty 18"
              },
              successBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "*Click* The lock opens!"
                  }
                }
              ],
              failureBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "The lock remains secure."
                  }
                }
              ]
            }
          ]
        }
      },
      scene: {},
      runtime
    };
    |}]

let%expect_test "multiple_states" =
  let story =
    {|@scene

## start
Welcome!
-> main

## main
Main area.

## end
Game over.|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export default {
      type: "scene",
      states: {
        start: {
          name: "start",
          blocks: [
            {
              type: "narration",
              content: {
                text: "Welcome!"
              }
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
              content: {
                text: "Main area."
              }
            }
          ]
        },
        end: {
          name: "end",
          blocks: [
            {
              type: "narration",
              content: {
                text: "Game over."
              }
            }
          ]
        }
      },
      scene: {},
      runtime
    };
    |}]

let%expect_test "localization_example" =
  let story =
    {|@scene
@background tavern_interior

## start
%{tavern.entrance.welcome}
Welcome to the Drunken Griffin!

Bartender: %{npc.bartender.greeting}
"What can I get you, traveler?"

* %{choice.order_drink} [Order a drink] -> drink
* %{choice.ask_rumors} [Ask about rumors] -> rumors

## drink
%{tavern.drink.response}
The bartender pours you a frothy ale.

## rumors
%{tavern.rumors.intro}
The bartender leans in closer...

Bartender: %{npc.bartender.secret}
"Strange things have been happening in the old mines..."|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export default {
      type: "scene",
      states: {
        start: {
          name: "start",
          blocks: [
            {
              type: "narration",
              content: {
                locale_key: "tavern.entrance.welcome",
                text: "Welcome to the Drunken Griffin!"
              }
            },
            {
              type: "dialogue",
              speaker: "Bartender",
              content: {
                locale_key: "npc.bartender.greeting",
                text: ""What can I get you, traveler?""
              }
            },
            {
              type: "narration",
              content: {
                text: "* %{choice.order_drink} [Order a drink] -> drink"
              }
            },
            {
              type: "narration",
              content: {
                text: "* %{choice.ask_rumors} [Ask about rumors] -> rumors"
              }
            }
          ]
        },
        drink: {
          name: "drink",
          blocks: [
            {
              type: "narration",
              content: {
                locale_key: "tavern.drink.response",
                text: "The bartender pours you a frothy ale."
              }
            }
          ]
        },
        rumors: {
          name: "rumors",
          blocks: [
            {
              type: "narration",
              content: {
                locale_key: "tavern.rumors.intro",
                text: "The bartender leans in closer..."
              }
            },
            {
              type: "dialogue",
              speaker: "Bartender",
              content: {
                locale_key: "npc.bartender.secret",
                text: ""Strange things have been happening in the old mines...""
              }
            }
          ]
        }
      },
      scene: {
        background: "tavern_interior"
      },
      runtime
    };
    |}]

let%expect_test "simple_story_example" =
  let story =
    {|@scene

## tavern_entrance

The tavern is dimly lit and filled with the smell of ale.

Bartender: "Welcome, stranger! What brings you here?"

* [Order a drink] -> order_drink
* [Ask about rumors] -> ask_rumors
* [Leave quietly] -> exit

## order_drink

You order a mug of ale.

[if gold >= 5]
  Bartender: "That'll be 5 gold pieces."
  $gold = gold - 5
  +ale
  -> drink_received

[if gold < 5]
  Bartender: "You don't have enough gold!"
  -> tavern_entrance

## drink_received

You receive your drink and take a sip.

@play_sound gulp.mp3

The ale is surprisingly good!

-> tavern_entrance

## ask_rumors

Bartender: "Well, there's talk of a dragon in the mountains..."

? perception check DC 15
  => You notice the bartender seems nervous when mentioning the dragon.
  =| The bartender continues polishing glasses.

-> tavern_entrance

## exit

You leave the tavern.

@scene_end|}
  in
  print_story_object story;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export default {
      type: "scene",
      states: {
        tavern_entrance: {
          name: "tavern_entrance",
          blocks: [
            {
              type: "narration",
              content: {
                text: "The tavern is dimly lit and filled with the smell of ale."
              }
            },
            {
              type: "dialogue",
              speaker: "Bartender",
              content: {
                text: ""Welcome, stranger! What brings you here?""
              }
            },
            {
              target: "order_drink",
              type: "choice",
              text: {
                text: "Order a drink"
              }
            },
            {
              target: "ask_rumors",
              type: "choice",
              text: {
                text: "Ask about rumors"
              }
            },
            {
              target: "exit",
              type: "choice",
              text: {
                text: "Leave quietly"
              }
            }
          ]
        },
        order_drink: {
          name: "order_drink",
          blocks: [
            {
              type: "narration",
              content: {
                text: "You order a mug of ale."
              }
            },
            {
              type: "conditional",
              condition: {
                type: "binary",
                operator: ">=",
                left: {
                  type: "variable",
                  name: "gold"
                },
                right: 5
              },
              thenBlocks: [
                {
                  type: "dialogue",
                  speaker: "Bartender",
                  content: {
                    text: ""That'll be 5 gold pieces.""
                  }
                },
                {
                  type: "variableSet",
                  name: "gold",
                  value: "gold - 5"
                },
                {
                  type: "itemAdd",
                  item: "ale"
                },
                {
                  type: "transition",
                  target: "drink_received"
                },
                {
                  type: "conditional",
                  condition: {
                    type: "binary",
                    operator: "<",
                    left: {
                      type: "variable",
                      name: "gold"
                    },
                    right: 5
                  },
                  thenBlocks: [
                    {
                      type: "dialogue",
                      speaker: "Bartender",
                      content: {
                        text: ""You don't have enough gold!""
                      }
                    },
                    {
                      type: "transition",
                      target: "tavern_entrance"
                    }
                  ]
                }
              ]
            }
          ]
        },
        drink_received: {
          name: "drink_received",
          blocks: [
            {
              type: "narration",
              content: {
                text: "You receive your drink and take a sip."
              }
            },
            {
              type: "directive",
              command: "play_sound",
              params: "gulp.mp3"
            },
            {
              type: "narration",
              content: {
                text: "The ale is surprisingly good!"
              }
            },
            {
              type: "transition",
              target: "tavern_entrance"
            }
          ]
        },
        ask_rumors: {
          name: "ask_rumors",
          blocks: [
            {
              type: "dialogue",
              speaker: "Bartender",
              content: {
                text: ""Well, there's talk of a dragon in the mountains...""
              }
            },
            {
              type: "skillCheck",
              skillType: "perception",
              difficulty: 15,
              description: {
                text: "perception check DC 15"
              },
              successBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "You notice the bartender seems nervous when mentioning the dragon."
                  }
                }
              ],
              failureBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "The bartender continues polishing glasses."
                  }
                },
                {
                  type: "transition",
                  target: "tavern_entrance"
                }
              ]
            }
          ]
        },
        exit: {
          name: "exit",
          blocks: [
            {
              type: "narration",
              content: {
                text: "You leave the tavern."
              }
            },
            {
              type: "directive",
              command: "scene_end",
              params: ""
            }
          ]
        }
      },
      scene: {},
      runtime
    };
    |}]

let%expect_test "manual_ast_conditional" =
  (* Manually create AST to test codegen *)
  let condition_ast =
    Ast.BinaryOp
      { op = Ast.Gte; left = Ast.Variable "$gold"; right = Ast.Literal (Ast.Int 50) }
  in
  let then_blocks = [ Ast.Narration { text = "You feel wealthy!"; locale_key = None } ] in
  let else_blocks =
    [ Ast.Narration { text = "You need more gold."; locale_key = None } ]
  in
  let conditional_block =
    Ast.Conditional
      { condition = condition_ast; then_blocks; else_blocks = Some else_blocks }
  in
  let state = { Ast.name = "treasury"; blocks = [ conditional_block ] } in
  let story =
    {
      Ast.metadata =
        {
          story_type = Ast.Scene { background = None; music = None };
          tags = [];
          uses = [];
        };
      states = [ state ];
    }
  in
  let js = Codegen.compile_to_js ~story_id:"test" story in
  let normalized = generate_js_normalized_from_js js in
  print_endline normalized;
  [%expect
    {|
    import { runtime } from "@narratoric/core";

    export default {
      type: "scene",
      states: {
        treasury: {
          name: "treasury",
          blocks: [
            {
              elseBlocks: [
                {
                  type: "narration",
                  content: {
                    text: "You need more gold."
                  }
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
                  content: {
                    text: "You feel wealthy!"
                  }
                }
              ]
            }
          ]
        }
      },
      scene: {},
      runtime
    };
    |}]
