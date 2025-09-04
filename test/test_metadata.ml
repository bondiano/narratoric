(** OUnit tests for story metadata parsing and code generation **)

open Base
open OUnit2
module Lexer = Narratoric.Story.Lexer
module Parser = Narratoric.Story.Parser
module Codegen = Narratoric.Story.Codegen

(** Helper to assert successful parsing and code generation **)
let assert_successful_compilation story_code ~story_id ~expected_contains ~msg =
  let tokens = Lexer.lex_story story_code in
  match Parser.parse ~filename:"test.story" tokens with
  | Error e -> assert_failure (Printf.sprintf "%s: Parse error: %s" msg e.message)
  | Ok ast ->
      let js = Codegen.compile_to_js ~story_id ast in
      List.iter expected_contains ~f:(fun substring ->
          assert_bool
            (Printf.sprintf "%s: Generated JS should contain '%s', got: %s" msg substring
               js )
            (String.is_substring js ~substring) )

(** Test scene type with background and music **)
let test_scene_type _ =
  let story =
    {|@scene
@background tavern_interior
@music crowd_chatter

## start
The tavern is bustling with activity.|}
  in
  assert_successful_compilation story ~story_id:"test"
    ~expected_contains:
      [ "type: \"scene\""; "tavern_interior"; "crowd_chatter"; "The tavern is bustling" ]
    ~msg:"Scene story should compile successfully"

(** Test NPC type **)
let test_npc_type _ =
  let story = {|@npc bartender

## greeting
Bartender: Welcome to my tavern!|} in
  assert_successful_compilation story ~story_id:"test"
    ~expected_contains:[ "type: \"npc\""; "bartender"; "Welcome to my tavern" ]
    ~msg:"NPC story should compile successfully"

(** Test story with tags and plugins **)
let test_tags_and_plugins _ =
  let story =
    {|@scene
@tags combat, social
@uses skills, inventory

## start
You enter the room.|}
  in
  assert_successful_compilation story ~story_id:"test"
    ~expected_contains:
      [
        "type: \"scene\""; "combat"; "social"; "skills"; "inventory"; "You enter the room";
      ]
    ~msg:"Story with tags and plugins should compile successfully"

(** Test quest type with all quest-specific directives **)
let test_quest_type _ =
  let story =
    {|@quest find_treasure
@objective Find the hidden treasure
@description A quest to find ancient treasure
@success_description You found the treasure!
@failed_description The treasure remains hidden

## start
Begin your search.|}
  in
  assert_successful_compilation story ~story_id:"test"
    ~expected_contains:
      [
        "type: \"quest\"";
        "find_treasure";
        "Find the hidden treasure";
        "Begin your search";
      ]
    ~msg:"Quest story should compile successfully"

(** Test merchant type with initial gold **)
let test_merchant_type _ =
  let story =
    {|@merchant shopkeeper
@initial_gold 500

## greeting
Welcome to my shop!|}
  in
  assert_successful_compilation story ~story_id:"test"
    ~expected_contains:[ "type: \"merchant\""; "shopkeeper"; "500"; "Welcome to my shop" ]
    ~msg:"Merchant story should compile successfully"

let suite =
  "Story Metadata Tests"
  >::: [
         "test_scene_type" >:: test_scene_type;
         "test_npc_type" >:: test_npc_type;
         "test_tags_and_plugins" >:: test_tags_and_plugins;
         "test_quest_type" >:: test_quest_type;
         "test_merchant_type" >:: test_merchant_type;
       ]

let () = run_test_tt_main suite
