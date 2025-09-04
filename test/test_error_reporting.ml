(** OUnit tests for enhanced error reporting with line and column information *)

open Base
open OUnit2
module Lexer = Narratoric.Story.Lexer
module Parser = Narratoric.Story.Parser

(** Helper to assert parse error with specific message content *)
let assert_parse_error story_code ~expected_substring ~msg =
  let tokens = Lexer.lex_story story_code in
  match Parser.parse ~filename:"test.story" tokens with
  | Ok _ -> assert_failure (msg ^ ": Expected error but parsing succeeded")
  | Error e ->
      assert_bool
        (Printf.sprintf "%s: Error message should contain '%s', got: %s" msg
           expected_substring e.message )
        (String.is_substring e.message ~substring:expected_substring);
      assert_bool "Error should have line number" (e.line > 0);
      assert_bool "Error should have column number" (e.column > 0)

let test_missing_story_type _ =
  let story = {|## start
This story has no type.|} in
  assert_parse_error story ~expected_substring:"Story type not specified"
    ~msg:"Should report missing story type"

let test_invalid_directive_for_type _ =
  let story = {|@scene
@initial_gold 100

## start
Scene can't have initial_gold.|} in
  assert_parse_error story
    ~expected_substring:"@initial_gold can only be used with @merchant"
    ~msg:"Should reject initial_gold in scene"

(** Test background directive in wrong story type *)
let test_background_in_npc _ =
  let story = {|@npc bartender
@background tavern

## greeting
Hello!|} in
  assert_parse_error story ~expected_substring:"@background can only be used with @scene"
    ~msg:"Should reject background in NPC story"

(** Test invalid initial_gold value *)
let test_invalid_initial_gold _ =
  let story =
    {|@merchant shopkeeper
@initial_gold not_a_number

## greeting
Welcome to my shop!|}
  in
  assert_parse_error story ~expected_substring:"not a valid number"
    ~msg:"Should reject non-numeric initial_gold"

(** Test unknown directive *)
let test_unknown_directive _ =
  let story =
    {|@scene
@unknown_directive some_value

## start
This has an unknown directive.|}
  in
  assert_parse_error story ~expected_substring:"Unknown directive '@unknown_directive'"
    ~msg:"Should report unknown directive"

(** Test quest directive in wrong story type *)
let test_quest_directive_in_scene _ =
  let story =
    {|@scene
@objective Find the treasure

## start
Scenes can't have objectives.|}
  in
  assert_parse_error story ~expected_substring:"@objective can only be used with @quest"
    ~msg:"Should reject objective in scene"

(** Test music directive in merchant story *)
let test_music_in_merchant _ =
  let story = {|@merchant trader
@music shop_theme

## greeting
Welcome!|} in
  assert_parse_error story ~expected_substring:"@music can only be used with @scene"
    ~msg:"Should reject music in merchant story"

(** Test description directive outside quest *)
let test_description_outside_quest _ =
  let story = {|@npc guard
@description A friendly guard

## talk
Guard: Hello there!|} in
  assert_parse_error story ~expected_substring:"@description can only be used with @quest"
    ~msg:"Should reject description in NPC story"

(** Test success_description outside quest *)
let test_success_description_outside_quest _ =
  let story = {|@scene
@success_description You win!

## start
A scene.|} in
  assert_parse_error story
    ~expected_substring:"@success_description can only be used with @quest"
    ~msg:"Should reject success_description in scene"

(** Test failed_description outside quest *)
let test_failed_description_outside_quest _ =
  let story = {|@merchant shop
@failed_description Quest failed

## start
Shop.|} in
  assert_parse_error story
    ~expected_substring:"@failed_description can only be used with @quest"
    ~msg:"Should reject failed_description in merchant"

(** Test that line and column numbers are reported *)
let test_error_position_reporting _ =
  let story = {|@scene
@background forest
@unknown_dir test

## start
Text.|} in
  let tokens = Lexer.lex_story story in
  match Parser.parse ~filename:"test.story" tokens with
  | Ok _ -> assert_failure "Expected error but parsing succeeded"
  | Error e ->
      (* Error should be on line 3 where @unknown_dir is *)
      assert_equal 3 e.line ~msg:"Error should be on line 3";
      assert_bool "Error should include line number in message"
        (String.is_substring e.message ~substring:"line 3")

let suite =
  "Error Reporting Tests"
  >::: [
         "test_missing_story_type" >:: test_missing_story_type;
         "test_invalid_directive_for_type" >:: test_invalid_directive_for_type;
         "test_background_in_npc" >:: test_background_in_npc;
         "test_invalid_initial_gold" >:: test_invalid_initial_gold;
         "test_unknown_directive" >:: test_unknown_directive;
         "test_quest_directive_in_scene" >:: test_quest_directive_in_scene;
         "test_music_in_merchant" >:: test_music_in_merchant;
         "test_description_outside_quest" >:: test_description_outside_quest;
         "test_success_description_outside_quest"
         >:: test_success_description_outside_quest;
         "test_failed_description_outside_quest" >:: test_failed_description_outside_quest;
         "test_error_position_reporting" >:: test_error_position_reporting;
       ]

let () = run_test_tt_main suite
