(** Story Mode Parser for the Narratoric DSL.

    This module converts a stream of tokens from the lexer into an Abstract Syntax Tree
    (AST) representing the story structure. *)

open Base
open Ast
module Lexer = Lexer

(** Parser state tracking current position in token stream *)
type parser_state = {
  tokens : Lexer.token list;  (** List of tokens to parse *)
  position : int;  (** Current position in the token list *)
}

(** Parser error information *)
type parse_error = {
  message : string;  (** Error description *)
  position : int;  (** Position in token stream where error occurred *)
}

(** Result type for parser operations *)
type 'a parse_result = ('a, parse_error) Result.t

let create_parser tokens = { tokens; position = 0 }

let is_at_end (state : parser_state) = state.position >= List.length state.tokens

let current_token (state : parser_state) = List.nth state.tokens state.position

let advance (state : parser_state) = { state with position = state.position + 1 }

let peek (state : parser_state) offset = List.nth state.tokens (state.position + offset)

(** Consume a specific token type or return an error *)
let consume state expected_token error_msg =
  match current_token state with
  | Some token when Poly.equal token expected_token -> Ok (advance state)
  | _ -> Error { message = error_msg; position = state.position }

(** Parse expressions for conditions and variable assignments. Currently returns a simple
    string literal - full expression parsing to be added. *)
let parse_expression tokens =
  (* Simple expression parser - just extracts the text for now *)
  (* This would need to be expanded for full expression parsing *)
  let rec collect_until_delimiter tokens delim acc =
    match tokens with
    | [] -> (String.concat ~sep:" " (List.rev acc), [])
    | Lexer.Newline :: rest when Poly.equal delim Lexer.Newline ->
        (String.concat ~sep:" " (List.rev acc), rest)
    | Lexer.Eof :: _ -> (String.concat ~sep:" " (List.rev acc), [])
    | token :: rest ->
        let text =
          match token with
          | Lexer.Text s -> s
          | Lexer.VariableSet (name, value) -> Printf.sprintf "%s = %s" name value
          | _ -> ""
        in
        if String.length text > 0 then collect_until_delimiter rest delim (text :: acc)
        else collect_until_delimiter rest delim acc
  in
  let expr_text, remaining = collect_until_delimiter tokens Lexer.Newline [] in
  (* For now, return a simple string literal - full expression parsing would be added
     here *)
  (Literal (String expr_text), remaining)

(** Parse a player choice block *)
let parse_choice state =
  match current_token state with
  | Some (Lexer.Choice text) ->
      let state = advance state in
      (* Check for arrow and target *)
      let target, state =
        match current_token state with
        | Some Lexer.Arrow -> (
            let state = advance state in
            match current_token state with
            | Some (Lexer.Text target_name) -> (Some target_name, advance state)
            | _ -> (None, state) )
        | _ -> (None, state)
      in
      Ok (Choice { text; target; condition = None }, state)
  | _ -> Error { message = "Expected choice"; position = state.position }

(** Parse a skill check with success/failure outcomes *)
let parse_skill_check state =
  match current_token state with
  | Some Lexer.SkillCheck ->
      let state = advance state in
      let description =
        match current_token state with Some (Lexer.Text desc) -> desc | _ -> ""
      in
      let state = if String.length description > 0 then advance state else state in

      (* Parse success and failure blocks *)
      let rec parse_skill_outcomes state success_blocks failure_blocks =
        match current_token state with
        | Some Lexer.SuccessArrow -> (
            let state = advance state in
            match current_token state with
            | Some (Lexer.Text text) ->
                let success_blocks = Narration text :: success_blocks in
                parse_skill_outcomes (advance state) success_blocks failure_blocks
            | _ -> parse_skill_outcomes state success_blocks failure_blocks )
        | Some Lexer.FailureArrow -> (
            let state = advance state in
            match current_token state with
            | Some (Lexer.Text text) ->
                let failure_blocks = Narration text :: failure_blocks in
                parse_skill_outcomes (advance state) success_blocks failure_blocks
            | _ -> parse_skill_outcomes state success_blocks failure_blocks )
        | Some Lexer.Newline ->
            parse_skill_outcomes (advance state) success_blocks failure_blocks
        | _ -> (List.rev success_blocks, List.rev failure_blocks, state)
      in

      let success_blocks, failure_blocks, state = parse_skill_outcomes state [] [] in

      Ok (SkillCheck { description; success_blocks; failure_blocks }, state)
  | _ -> Error { message = "Expected skill check"; position = state.position }

(** Parse a single block element within a state *)
let rec parse_block state =
  match current_token state with
  | None | Some Lexer.Eof ->
      Error { message = "Unexpected end of file"; position = state.position }
  | Some Lexer.Newline ->
      (* Skip empty lines *)
      parse_block (advance state)
  | Some (Lexer.StateMarker _) ->
      (* End of current state *)
      Error { message = "State marker"; position = state.position }
  | Some (Lexer.Text text) -> Ok (Narration text, advance state)
  | Some (Lexer.Speaker speaker) -> (
      let state = advance state in
      match current_token state with
      | Some (Lexer.Text text) -> Ok (Dialogue { speaker; text }, advance state)
      | _ -> Ok (Narration speaker, state) )
  | Some (Lexer.Choice _) -> parse_choice state
  | Some (Lexer.Condition cond_text) ->
      (* Simple conditional - would need more complex parsing *)
      let state = advance state in
      let rec collect_blocks state blocks =
        match parse_block state with
        | Ok (block, new_state) -> collect_blocks new_state (block :: blocks)
        | Error _ -> (List.rev blocks, state)
      in
      let then_blocks, state = collect_blocks state [] in
      Ok
        ( Conditional
            { condition = Literal (String cond_text); then_blocks; else_blocks = None },
          state )
  | Some Lexer.SkillCheck -> parse_skill_check state
  | Some (Lexer.VariableSet (name, value)) ->
      Ok (VariableSet { name; value = Literal (String value) }, advance state)
  | Some (Lexer.AddItem item) -> Ok (ItemAdd item, advance state)
  | Some (Lexer.RemoveItem item) -> Ok (ItemRemove item, advance state)
  | Some (Lexer.Directive (command, params)) ->
      Ok (Directive { command; params }, advance state)
  | Some Lexer.Arrow -> (
      let state = advance state in
      match current_token state with
      | Some (Lexer.Text target) -> Ok (Transition target, advance state)
      | _ ->
          Error { message = "Expected state name after arrow"; position = state.position }
      )
  | Some _ ->
      (* Skip unknown tokens *)
      parse_block (advance state)

(** Parse multiple blocks until a state marker or end of tokens *)
let rec parse_blocks state acc =
  if is_at_end state then Ok (List.rev acc, state)
  else
    match current_token state with
    | Some (Lexer.StateMarker _) | Some Lexer.Eof -> Ok (List.rev acc, state)
    | _ -> (
        match parse_block state with
        | Ok (block, new_state) -> parse_blocks new_state (block :: acc)
        | Error e when String.equal e.message "State marker" -> Ok (List.rev acc, state)
        | Error e -> Error e )

(** Parse a complete state including its name and all blocks *)
let parse_state state =
  match current_token state with
  | Some (Lexer.StateMarker name) -> (
      let state = advance state in
      match parse_blocks state [] with
      | Ok (blocks, new_state) -> Ok ({ name; blocks }, new_state)
      | Error e -> Error e )
  | _ -> Error { message = "Expected state marker"; position = state.position }

(** Parse all states in the story *)
let rec parse_states state acc =
  if is_at_end state then Ok (List.rev acc)
  else
    match current_token state with
    | Some Lexer.Eof -> Ok (List.rev acc)
    | Some Lexer.Newline -> parse_states (advance state) acc
    | _ -> (
        match parse_state state with
        | Ok (parsed_state, new_state) -> parse_states new_state (parsed_state :: acc)
        | Error e -> Error e )

(** Main parse function that converts tokens to an AST.
    @param tokens List of tokens from the lexer
    @return Either a story AST or a parse error *)
let parse tokens =
  let state = create_parser tokens in
  parse_states state []
