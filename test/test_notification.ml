(** Test for notification directive *)

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

let%expect_test "notification_directive_with_localization" =
  let story =
    {|@scene

## start
Welcome to the tavern!

@show_notification %{ui.notification.money_lost} Oh my u take all money

The bartender looks at you suspiciously.

@show_notification You gained experience points

-> end

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
                text: "Welcome to the tavern!"
              }
            },
            {
              type: "notification",
              content: {
                locale_key: "ui.notification.money_lost",
                text: "Oh my u take all money"
              }
            },
            {
              type: "narration",
              content: {
                text: "The bartender looks at you suspiciously."
              }
            },
            {
              type: "notification",
              content: {
                text: "You gained experience points"
              }
            },
            {
              type: "transition",
              target: "end"
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