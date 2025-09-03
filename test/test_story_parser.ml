open Base
open OUnit2
module Lexer = Narratoric.Story.Lexer
module Parser = Narratoric.Story.Parser
module Ast = Narratoric.Story.Ast

let parse_string input =
  let tokens = Lexer.lex_story input in
  Parser.parse tokens

let test_simple_narration _ =
  let input = "## start\nThis is narration text." in
  match parse_string input with
  | Ok states -> (
      assert_equal 1 (List.length states);
      let state = List.hd_exn states in
      assert_equal "start" state.name;
      assert_equal 1 (List.length state.blocks);
      match List.hd_exn state.blocks with
      | Ast.Narration text -> assert_equal "This is narration text." text
      | _ -> assert_failure "Expected narration block" )
  | Error e -> assert_failure e.message

let test_dialogue _ =
  let input = {|## dialogue_test
John: "Hello, world!"|} in
  match parse_string input with
  | Ok states -> (
      let state = List.hd_exn states in
      assert_equal "dialogue_test" state.name;
      match List.hd_exn state.blocks with
      | Ast.Dialogue { speaker; text } ->
          assert_equal "John" speaker;
          assert_equal "\"Hello, world!\"" text
      | _ -> assert_failure "Expected dialogue block" )
  | Error e -> assert_failure e.message

let test_choice _ =
  let input = {|## choices
* [Go left] -> left_path
* [Go right] -> right_path|} in
  match parse_string input with
  | Ok states ->
      let state = List.hd_exn states in
      assert_equal 2 (List.length state.blocks);
      List.iter state.blocks ~f:(function
        | Ast.Choice { text; target; _ } ->
            assert_bool "Choice should have text" (String.length text > 0);
            assert_bool "Choice should have target" (Option.is_some target)
        | _ -> assert_failure "Expected choice blocks" )
  | Error e -> assert_failure e.message

let test_variables _ =
  let input = {|## variables
$gold = 100
$name = "Hero"|} in
  match parse_string input with
  | Ok states -> (
      let state = List.hd_exn states in
      assert_equal 2 (List.length state.blocks);
      match List.hd state.blocks with
      | Some (Ast.VariableSet { name; _ }) -> assert_equal "gold" name
      | _ -> assert_failure "Expected variable set" )
  | Error e -> assert_failure e.message

let test_items _ =
  let input = {|## items
+sword
-potion|} in
  match parse_string input with
  | Ok states -> (
      let state = List.hd_exn states in
      assert_equal 2 (List.length state.blocks);
      match state.blocks with
      | [ Ast.ItemAdd item1; Ast.ItemRemove item2 ] ->
          assert_equal "sword" item1;
          assert_equal "potion" item2
      | _ -> assert_failure "Expected item operations" )
  | Error e -> assert_failure e.message

let test_directives _ =
  let input = {|## directives
@play_sound thunder.mp3
@scene_end|} in
  match parse_string input with
  | Ok states -> (
      let state = List.hd_exn states in
      match List.hd state.blocks with
      | Some (Ast.Directive { command; params }) ->
          assert_equal "play_sound" command;
          assert_equal "thunder.mp3" params
      | _ -> assert_failure "Expected directive" )
  | Error e -> assert_failure e.message

let test_transitions _ =
  let input = {|## transitions
Some text here.
-> next_state|} in
  match parse_string input with
  | Ok states -> (
      let state = List.hd_exn states in
      assert_equal 2 (List.length state.blocks);
      match List.nth state.blocks 1 with
      | Some (Ast.Transition target) -> assert_equal "next_state" target
      | _ -> assert_failure "Expected transition" )
  | Error e -> assert_failure e.message

let test_skill_check _ =
  let input =
    {|## skill_test
? perception check DC 15
  => You notice something!
  =| You see nothing unusual.|}
  in
  match parse_string input with
  | Ok states -> (
      let state = List.hd_exn states in
      match List.hd state.blocks with
      | Some (Ast.SkillCheck { description; success_blocks; failure_blocks }) ->
          assert_equal "perception check DC 15" description;
          assert_bool "Should have success block" (List.length success_blocks > 0);
          assert_bool "Should have failure block" (List.length failure_blocks > 0)
      | _ -> assert_failure "Expected skill check" )
  | Error e -> assert_failure e.message

let test_conditional _ =
  let input = {|## conditional_test
[if gold >= 10]
  You can afford it.
  -> shop|} in
  match parse_string input with
  | Ok states -> (
      let state = List.hd_exn states in
      match List.hd state.blocks with
      | Some (Ast.Conditional { then_blocks; _ }) ->
          assert_bool "Should have then blocks" (List.length then_blocks > 0)
      | _ -> assert_failure "Expected conditional" )
  | Error e -> assert_failure e.message

let test_multiple_states _ =
  let input =
    {|## state1
First state text.

## state2
Second state text.

## state3
Third state text.|}
  in
  match parse_string input with
  | Ok states ->
      assert_equal 3 (List.length states);
      List.iter states ~f:(fun state ->
          assert_bool "State should have name" (String.length state.name > 0);
          assert_bool "State should have blocks" (List.length state.blocks > 0) )
  | Error e -> assert_failure e.message

let test_complex_scene _ =
  let input =
    {|## tavern_entrance

The tavern is dimly lit.

Bartender: "Welcome!"

* [Order ale] -> order_drink
* [Leave] -> exit

[if has_money]
  You jingle your coin purse.

@play_sound ambient_tavern|}
  in
  match parse_string input with
  | Ok states ->
      let state = List.hd_exn states in
      assert_equal "tavern_entrance" state.name;
      assert_bool "Should have multiple blocks" (List.length state.blocks >= 5)
  | Error e -> assert_failure e.message

let suite =
  "Story Parser Tests"
  >::: [
         "test_simple_narration" >:: test_simple_narration;
         "test_dialogue" >:: test_dialogue;
         "test_choice" >:: test_choice;
         "test_variables" >:: test_variables;
         "test_items" >:: test_items;
         "test_directives" >:: test_directives;
         "test_transitions" >:: test_transitions;
         "test_skill_check" >:: test_skill_check;
         "test_conditional" >:: test_conditional;
         "test_multiple_states" >:: test_multiple_states;
         "test_complex_scene" >:: test_complex_scene;
       ]

let () = run_test_tt_main suite
