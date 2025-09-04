(** OUnit tests for JavaScript code generation *)

open Base
open OUnit2
module Lexer = Narratoric.Story.Lexer
module Parser = Narratoric.Story.Parser
module Codegen = Narratoric.Story.Codegen
module Ast = Narratoric.Story.Ast

let generate_js story_code ~story_id =
  let tokens = Lexer.lex_story story_code in
  match Parser.parse tokens with
  | Error e -> Error e.message
  | Ok story -> Ok (Codegen.compile_to_js ~story_id story)

let assert_js_contains js ~substring ~msg =
  assert_bool msg (String.is_substring js ~substring)

let test_simple_narration _ =
  let story = "## start\nThis is a simple story." in
  match generate_js story ~story_id:"test" with
  | Error msg -> assert_failure msg
  | Ok js ->
      assert_js_contains js ~substring:"start" ~msg:"Should contain start state";
      assert_js_contains js ~substring:"This is a simple story."
        ~msg:"Should contain story text"

(** Test multiple states *)
let test_multiple_states _ =
  let story = {|## start
Welcome!

## main
Main area.|} in
  match generate_js story ~story_id:"test" with
  | Error msg -> assert_failure msg
  | Ok js ->
      assert_js_contains js ~substring:"start" ~msg:"Should contain start state";
      assert_js_contains js ~substring:"main" ~msg:"Should contain main state";
      assert_js_contains js ~substring:"Welcome!" ~msg:"Should contain start text";
      assert_js_contains js ~substring:"Main area." ~msg:"Should contain main text"

(** Test basic JavaScript module structure *)
let test_module_structure _ =
  let story = "## start\nHello world." in
  match generate_js story ~story_id:"test_story" with
  | Error msg -> assert_failure msg
  | Ok js ->
      assert_js_contains js ~substring:"import { runtime } from \"@narratoric/core\""
        ~msg:"Should import runtime";
      assert_js_contains js ~substring:"export const story" ~msg:"Should export story"

(** Test that parser produces expected AST for narration *)
let test_narration_ast _ =
  let story = "## start\nThis is narration text." in
  let tokens = Lexer.lex_story story in
  match Parser.parse tokens with
  | Error e -> assert_failure e.message
  | Ok states ->
      assert_equal 1 (List.length states) ~msg:"Should have one state";
      let state = List.hd_exn states in
      assert_equal "start" state.name ~msg:"State should be named 'start'";
      assert_equal 1 (List.length state.blocks) ~msg:"Should have one block";
      let block = List.hd_exn state.blocks in
      assert_equal (Ast.Narration "This is narration text.") block
        ~msg:"Should contain narration block"

(** Test that parser handles multiple states *)
let test_multiple_states_ast _ =
  let story = {|## start
Welcome!

## end
Goodbye!|} in
  let tokens = Lexer.lex_story story in
  match Parser.parse tokens with
  | Error e -> assert_failure e.message
  | Ok states ->
      assert_equal 2 (List.length states) ~msg:"Should have two states";
      let first_state = List.hd_exn states in
      let second_state = List.nth_exn states 1 in
      assert_equal "start" first_state.name ~msg:"First state should be 'start'";
      assert_equal "end" second_state.name ~msg:"Second state should be 'end'"

let suite =
  "Story Codegen OUnit Tests"
  >::: [
         "test_simple_narration" >:: test_simple_narration;
         "test_multiple_states" >:: test_multiple_states;
         "test_module_structure" >:: test_module_structure;
         "test_narration_ast" >:: test_narration_ast;
         "test_multiple_states_ast" >:: test_multiple_states_ast;
       ]

let () = run_test_tt_main suite
