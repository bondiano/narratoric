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

(** Parse expressions for conditions and variable assignments. *)
let rec parse_expression tokens =
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
  let parsed_expr = parse_condition_expression expr_text in
  (parsed_expr, remaining)

(** Parse condition expression string into structured AST *)
and parse_condition_expression expr_text =
  (* Remove "if " prefix if present *)
  let clean_expr =
    if String.is_prefix expr_text ~prefix:"if " then String.drop_prefix expr_text 3
    else expr_text
  in

  (* Enhanced parser for complex expressions *)
  parse_logical_expression clean_expr

(** Parse logical expressions with 'and', 'or' operators *)
and parse_logical_expression expr =
  (* Handle 'and' with higher precedence than 'or' *)
  let or_parts =
    String.split_on_chars expr ~on:[ ';' ] |> List.hd_exn |> fun s ->
    (* Split on 'or' but handle parentheses *)
    parse_or_expression s
  in
  or_parts

and parse_or_expression expr =
  let parts = split_respecting_parens expr " or " in
  match parts with
  | [ single ] -> parse_and_expression single
  | multiple ->
      List.fold multiple
        ~init:(parse_and_expression (List.hd_exn multiple))
        ~f:(fun acc part ->
          BinaryOp { op = Or; left = acc; right = parse_and_expression part } )

and parse_and_expression expr =
  let parts = split_respecting_parens expr " and " in
  match parts with
  | [ single ] -> parse_comparison_expression single
  | multiple ->
      List.fold multiple
        ~init:(parse_comparison_expression (List.hd_exn multiple))
        ~f:(fun acc part ->
          BinaryOp { op = And; left = acc; right = parse_comparison_expression part } )

and parse_comparison_expression expr =
  let expr = String.strip expr in
  (* Remove outer parentheses if they wrap the entire expression *)
  let expr =
    if String.is_prefix expr ~prefix:"(" && String.is_suffix expr ~suffix:")" then
      String.sub expr ~pos:1 ~len:(String.length expr - 2) |> String.strip
    else expr
  in

  (* Simple parser for basic comparisons: "$gold >= 50" *)
  let tokens =
    String.split expr ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  match tokens with
  | [ var; op; value ] ->
      let left = Variable var in
      let right =
        match Int.of_string value with
        | n -> Literal (Int n)
        | exception _ -> Literal (String value)
      in
      let binary_op = parse_operator op in
      BinaryOp { op = binary_op; left; right }
  | _ ->
      (* For very complex expressions, fallback to string *)
      Literal (String expr)

and parse_operator = function
  | ">=" -> Gte
  | "<=" -> Lte
  | "==" -> Eq
  | "!=" -> Neq
  | ">" -> Gt
  | "<" -> Lt
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | _ -> Eq (* default *)

(** Split string on delimiter while respecting parentheses *)
and split_respecting_parens text delimiter =
  let len = String.length text in
  let delim_len = String.length delimiter in
  let rec split_aux pos depth acc current =
    if pos >= len then
      let final_part = String.of_char_list (List.rev current) in
      List.rev (final_part :: acc)
    else
      let char = text.[pos] in
      match char with
      | '(' -> split_aux (pos + 1) (depth + 1) acc (char :: current)
      | ')' -> split_aux (pos + 1) (depth - 1) acc (char :: current)
      | _ ->
          if
            depth = 0
            && pos + delim_len <= len
            && String.equal (String.sub text ~pos ~len:delim_len) delimiter
          then
            let part = String.of_char_list (List.rev current) |> String.strip in
            split_aux (pos + delim_len) depth (part :: acc) []
          else split_aux (pos + 1) depth acc (char :: current)
  in
  match split_aux 0 0 [] [] with [] -> [ text ] | result -> result

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
      let state = advance state in
      let rec collect_blocks state blocks =
        match parse_block state with
        | Ok (block, new_state) -> collect_blocks new_state (block :: blocks)
        | Error _ -> (List.rev blocks, state)
      in
      let then_blocks, state = collect_blocks state [] in
      let condition_ast = parse_condition_expression cond_text in
      Ok
        (Conditional { condition = condition_ast; then_blocks; else_blocks = None }, state)
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
