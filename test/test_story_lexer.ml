open Base
open OUnit2
module Lexer = Narratoric.Story.Lexer
open Lexer

let show_token = function
  | Text s -> Printf.sprintf "Text(%s)" s
  | Choice s -> Printf.sprintf "Choice(%s)" s
  | Arrow -> "Arrow"
  | StateMarker s -> Printf.sprintf "StateMarker(%s)" s
  | Directive (cmd, params) -> Printf.sprintf "Directive(%s, %s)" cmd params
  | Condition s -> Printf.sprintf "Condition(%s)" s
  | SkillCheck -> "SkillCheck"
  | SuccessArrow -> "SuccessArrow"
  | FailureArrow -> "FailureArrow"
  | VariableSet (var, value) -> Printf.sprintf "VariableSet(%s, %s)" var value
  | AddItem s -> Printf.sprintf "AddItem(%s)" s
  | RemoveItem s -> Printf.sprintf "RemoveItem(%s)" s
  | Speaker s -> Printf.sprintf "Speaker(%s)" s
  | Newline -> "Newline"
  | Comment s -> Printf.sprintf "Comment(%s)" s
  | Eof -> "Eof"

let equal_token t1 t2 =
  match (t1, t2) with
  | Text s1, Text s2 -> String.equal s1 s2
  | Choice s1, Choice s2 -> String.equal s1 s2
  | Arrow, Arrow -> true
  | StateMarker s1, StateMarker s2 -> String.equal s1 s2
  | Directive (c1, p1), Directive (c2, p2) -> String.equal c1 c2 && String.equal p1 p2
  | Condition s1, Condition s2 -> String.equal s1 s2
  | SkillCheck, SkillCheck -> true
  | SuccessArrow, SuccessArrow -> true
  | FailureArrow, FailureArrow -> true
  | VariableSet (v1, val1), VariableSet (v2, val2) ->
      String.equal v1 v2 && String.equal val1 val2
  | AddItem s1, AddItem s2 -> String.equal s1 s2
  | RemoveItem s1, RemoveItem s2 -> String.equal s1 s2
  | Speaker s1, Speaker s2 -> String.equal s1 s2
  | Newline, Newline -> true
  | Comment s1, Comment s2 -> String.equal s1 s2
  | Eof, Eof -> true
  | _ -> false

(* Test helper to compare token lists *)
let assert_tokens_equal expected actual =
  let expected_str = List.map expected ~f:show_token |> String.concat ~sep:", " in
  let actual_str = List.map actual ~f:show_token |> String.concat ~sep:", " in
  assert_equal
    ~printer:(fun x -> x)
    ~msg:"Token lists do not match" expected_str actual_str
    ~cmp:(fun _ _ -> List.equal equal_token expected actual)

let lex_story = Lexer.lex_story

let test_narration _ =
  let input = "This is simple narration text." in
  let expected = [ Text "This is simple narration text."; Eof ] in
  let actual = lex_story input in
  assert_tokens_equal expected actual

let test_choices _ =
  let input = "* [Enter the tavern] -> tavern_entrance" in
  let expected = [ Choice "Enter the tavern"; Arrow; Text "tavern_entrance"; Eof ] in
  let actual = lex_story input in
  assert_tokens_equal expected actual

let test_speaker _ =
  let input = "John: \"Hello, traveler!\"" in
  let expected = [ Speaker "John"; Text "\"Hello, traveler!\""; Eof ] in
  let actual = lex_story input in
  assert_tokens_equal expected actual

let test_state_markers _ =
  let input = "## entrance" in
  let expected = [ StateMarker "entrance"; Eof ] in
  let actual = lex_story input in
  assert_tokens_equal expected actual

let test_directives _ =
  let input = "@play_sound door_open" in
  let expected = [ Directive ("play_sound", "door_open"); Eof ] in
  let actual = lex_story input in
  assert_tokens_equal expected actual

let test_conditions _ =
  let input = "[if has_item(key)]" in
  let expected = [ Condition "if has_item(key)"; Eof ] in
  let actual = lex_story input in
  assert_tokens_equal expected actual

let test_skill_checks _ =
  let input =
    {|? perception check DC 15
  => You notice a hidden door
  =| Nothing unusual here|}
  in
  let expected =
    [
      SkillCheck;
      Text "perception check DC 15";
      Newline;
      SuccessArrow;
      Text "You notice a hidden door";
      Newline;
      FailureArrow;
      Text "Nothing unusual here";
      Eof;
    ]
  in
  let actual = lex_story input in
  assert_tokens_equal expected actual

let test_variables _ =
  let input = "$gold = 100" in
  let expected = [ VariableSet ("gold", "100"); Eof ] in
  let actual = lex_story input in
  assert_tokens_equal expected actual

let test_inventory _ =
  let input = {|+rusty_sword
-health_potion|} in
  let expected = [ AddItem "rusty_sword"; Newline; RemoveItem "health_potion"; Eof ] in
  let actual = lex_story input in
  assert_tokens_equal expected actual

let test_single_line_comments _ =
  let input = {|(* This is a comment *)
Some text
(* Another comment *) More text|} in
  let expected = [ Newline; Text "Some text"; Newline; Text "More text"; Eof ] in
  let actual = lex_story input in
  assert_tokens_equal expected actual

let test_multi_line_comments _ =
  let input =
    {|(* This is a
multi-line
comment *)
Some text
(* Nested (* comments *) work *) More text|}
  in
  (* After skipping the multi-line comment, there's still a newline after the comment *)
  let expected = [ Newline; Text "Some text"; Newline; Text "More text"; Eof ] in
  let actual = lex_story input in
  assert_tokens_equal expected actual

let test_comments_with_tokens _ =
  let input =
    {|## state_name (* comment after state marker *)

(* Comment before narration *)
The tavern is busy.

(* Multi-line comment
   before choice *)
* [Enter] -> enter

@directive param (* comment after directive *)|}
  in
  let expected =
    [
      StateMarker "state_name";
      Newline;
      Newline;
      Newline;
      (* Extra newline after the comment *)
      Text "The tavern is busy.";
      Newline;
      Newline;
      Newline;
      (* Extra newline after the multi-line comment *)
      Choice "Enter";
      Arrow;
      Text "enter";
      Newline;
      Newline;
      Directive ("directive", "param");
      Eof;
    ]
  in
  let actual = lex_story input in
  assert_tokens_equal expected actual

let test_complex_scene _ =
  let input =
    {|## tavern_entrance

The tavern is dimly lit.

Bartender: "Welcome, stranger!"

* [Order a drink] -> order_drink
* [Ask about rumors] -> ask_rumors
* [Leave] -> exit

[if gold >= 5]
  You have enough gold for a drink.

@play_sound ambient_tavern|}
  in

  let expected =
    [
      StateMarker "tavern_entrance";
      Newline;
      Newline;
      Text "The tavern is dimly lit.";
      Newline;
      Newline;
      Speaker "Bartender";
      Text "\"Welcome, stranger!\"";
      Newline;
      Newline;
      Choice "Order a drink";
      Arrow;
      Text "order_drink";
      Newline;
      Choice "Ask about rumors";
      Arrow;
      Text "ask_rumors";
      Newline;
      Choice "Leave";
      Arrow;
      Text "exit";
      Newline;
      Newline;
      Condition "if gold >= 5";
      Newline;
      Text "You have enough gold for a drink.";
      Newline;
      Newline;
      Directive ("play_sound", "ambient_tavern");
      Eof;
    ]
  in
  let actual = lex_story input in
  assert_tokens_equal expected actual

let suite =
  "Story Mode Lexer Tests"
  >::: [
         "test_narration" >:: test_narration;
         "test_choices" >:: test_choices;
         "test_speaker" >:: test_speaker;
         "test_state_markers" >:: test_state_markers;
         "test_directives" >:: test_directives;
         "test_conditions" >:: test_conditions;
         "test_skill_checks" >:: test_skill_checks;
         "test_variables" >:: test_variables;
         "test_inventory" >:: test_inventory;
         "test_single_line_comments" >:: test_single_line_comments;
         "test_multi_line_comments" >:: test_multi_line_comments;
         "test_comments_with_tokens" >:: test_comments_with_tokens;
         "test_complex_scene" >:: test_complex_scene;
       ]

let () = run_test_tt_main suite
