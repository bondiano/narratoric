(** Story Mode Parser for the Narratoric DSL.

    This module converts a stream of tokens from the lexer into an Abstract Syntax Tree
    (AST) representing the story structure. *)

open Base
open Ast
module Lexer = Lexer

(** Position information for error reporting *)
type position_info = {
  line : int;  (** Line number (1-based) *)
  column : int;  (** Column number (1-based) *)
  token_index : int;  (** Index in token list *)
}

(** Parser state tracking current position in token stream *)
type parser_state = {
  tokens : Lexer.token list;  (** List of tokens to parse *)
  position : int;  (** Current position in the token list *)
  line : int;  (** Current line number (1-based) *)
  column : int;  (** Current column number (1-based) *)
  token_positions : position_info list;  (** Position info for each token *)
}

(** Parser error information *)
type parse_error = {
  message : string;  (** Error description *)
  position : int;  (** Position in token stream where error occurred *)
  line : int;  (** Line number where error occurred *)
  column : int;  (** Column number where error occurred *)
}

(** Result type for parser operations *)
type 'a parse_result = ('a, parse_error) Result.t

(** Build position info for all tokens *)
let build_token_positions tokens =
  let rec build_positions tokens line col acc =
    match tokens with
    | [] -> List.rev acc
    | Lexer.Newline :: rest ->
        let pos = { line; column = col; token_index = List.length acc } in
        build_positions rest (line + 1) 1 (pos :: acc)
    | _ :: rest ->
        let pos = { line; column = col; token_index = List.length acc } in
        build_positions rest line (col + 1) (pos :: acc)
  in
  build_positions tokens 1 1 []

let create_parser tokens =
  let positions = build_token_positions tokens in
  { tokens; position = 0; line = 1; column = 1; token_positions = positions }

let is_at_end (state : parser_state) = state.position >= List.length state.tokens

let current_token (state : parser_state) = List.nth state.tokens state.position

let get_current_position (state : parser_state) =
  match List.nth state.token_positions state.position with
  | Some pos -> (pos.line, pos.column)
  | None -> (state.line, state.column)

let advance (state : parser_state) =
  let new_pos = state.position + 1 in
  match List.nth state.token_positions new_pos with
  | Some pos -> { state with position = new_pos; line = pos.line; column = pos.column }
  | None -> { state with position = new_pos }

let peek (state : parser_state) offset = List.nth state.tokens (state.position + offset)

(** Skip newline tokens *)
let rec skip_newlines state =
  match current_token state with
  | Some Lexer.Newline -> skip_newlines (advance state)
  | _ -> state

(** Create an error with current position information *)
let make_error state message =
  let line, column = get_current_position state in
  { message; position = state.position; line; column }

(** Consume a specific token type or return an error *)
let consume state expected_token error_msg =
  match current_token state with
  | Some token when Poly.equal token expected_token -> Ok (advance state)
  | _ -> Error (make_error state error_msg)

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
      let choice_text = { text; locale_key = None } in
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
      Ok (Choice { text = choice_text; target; condition = None }, state)
  | _ -> Error (make_error state "Expected choice")

(** Parse skill check parameters from text Supports formats like:
    - "perception DC 15"
    - "agility check DC 20"
    - "strength 15"
    - "lockpicking difficulty 25" *)
let parse_skill_check_params text =
  let open String in
  let words = split text ~on:' ' |> List.filter ~f:(fun s -> not (is_empty s)) in
  let safe_int_of_string s = try Some (Int.of_string s) with _ -> None in
  let rec extract_skill_and_dc words skill_acc =
    match words with
    | [] -> (skill_acc, None)
    | "DC" :: dc_str :: _ -> (skill_acc, safe_int_of_string dc_str)
    | "difficulty" :: dc_str :: _ -> (skill_acc, safe_int_of_string dc_str)
    | num_str :: _
      when (not (String.is_empty num_str)) && Char.is_digit (String.get num_str 0) ->
        (skill_acc, safe_int_of_string num_str)
    | "check" :: rest -> extract_skill_and_dc rest skill_acc
    | word :: rest when String.equal skill_acc "" -> extract_skill_and_dc rest word
    | _ :: rest -> extract_skill_and_dc rest skill_acc
  in
  match words with [] -> ("", None) | _ -> extract_skill_and_dc words ""

(** Parse a skill check with success/failure outcomes *)
let rec parse_skill_check state =
  match current_token state with
  | Some Lexer.SkillCheck -> (
      let state = advance state in
      match current_token state with
      | Some (Lexer.Text desc) -> (
          let skill_type, difficulty_opt = parse_skill_check_params desc in
          match (skill_type, difficulty_opt) with
          | "", _ ->
              Error
                (make_error state
                   (Printf.sprintf
                      "Invalid skill check format: '%s'. Expected format like \
                       'perception DC 15' or 'agility 20'"
                      desc ) )
          | _, None ->
              Error
                (make_error state
                   (Printf.sprintf
                      "Missing difficulty in skill check: '%s'. Expected format like \
                       'perception DC 15' or 'agility 20'"
                      desc ) )
          | skill, Some diff ->
              let description = { text = desc; locale_key = None } in
              let state = advance state in
              parse_skill_check_blocks state skill diff description )
      | _ -> Error (make_error state "Expected skill check description") )
  | _ -> Error (make_error state "Expected skill check")

and parse_skill_check_blocks state skill_type difficulty description =
  let rec parse_skill_outcomes state success_blocks failure_blocks =
    match current_token state with
    | Some Lexer.SuccessArrow -> (
        let state = advance state in
        match current_token state with
        | Some (Lexer.Text text) ->
            let success_blocks =
              Narration { text; locale_key = None } :: success_blocks
            in
            parse_skill_outcomes (advance state) success_blocks failure_blocks
        | _ -> parse_skill_outcomes state success_blocks failure_blocks )
    | Some Lexer.FailureArrow -> (
        let state = advance state in
        match current_token state with
        | Some (Lexer.Text text) ->
            let failure_blocks =
              Narration { text; locale_key = None } :: failure_blocks
            in
            parse_skill_outcomes (advance state) success_blocks failure_blocks
        | _ -> parse_skill_outcomes state success_blocks failure_blocks )
    | Some Lexer.Newline ->
        parse_skill_outcomes (advance state) success_blocks failure_blocks
    | _ -> (List.rev success_blocks, List.rev failure_blocks, state)
  in

  let success_blocks, failure_blocks, state = parse_skill_outcomes state [] [] in
  Ok
    ( SkillCheck { skill_type; difficulty; description; success_blocks; failure_blocks },
      state )

(** Parse localizable text - check for LocaleKey token followed by Text token *)
let parse_localizable_text state =
  match current_token state with
  | Some (Lexer.LocaleKey key) -> (
      let state = advance state in
      (* Skip newlines after locale key *)
      let state = skip_newlines state in
      match current_token state with
      | Some (Lexer.Text text) -> Ok ({ text; locale_key = Some key }, advance state)
      | _ -> Error (make_error state "Expected text after locale key") )
  | Some (Lexer.Text text) -> Ok ({ text; locale_key = None }, advance state)
  | _ -> Error (make_error state "Expected text or locale key")

(** Parse a single block element within a state *)
let rec parse_block state =
  match current_token state with
  | None | Some Lexer.Eof -> Error (make_error state "Unexpected end of file")
  | Some Lexer.Newline ->
      (* Skip empty lines *)
      parse_block (advance state)
  | Some (Lexer.StateMarker _) ->
      (* End of current state *)
      Error (make_error state "State marker")
  | Some (Lexer.LocaleKey _) | Some (Lexer.Text _) -> (
      match parse_localizable_text state with
      | Ok (text, new_state) -> Ok (Narration text, new_state)
      | Error e -> Error e )
  | Some (Lexer.Speaker speaker) -> (
      let state = advance state in
      match parse_localizable_text state with
      | Ok (text, new_state) -> Ok (Dialogue { speaker; text }, new_state)
      | Error _ ->
          (* Fallback: treat speaker as narration *)
          Ok (Narration { text = speaker; locale_key = None }, state) )
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
  | Some (Lexer.Directive (command, params)) -> (
      match command with
      | "show_notification" -> (
          (* Parse notification text with possible locale key *)
          (* Create a temporary lexer state for the params *)
          let temp_tokens = Lexer.lex_story params in
          let temp_state = create_parser temp_tokens in
          match parse_localizable_text temp_state with
          | Ok (notification_text, _) -> Ok (Notification notification_text, advance state)
          | Error _ ->
              (* Fallback: treat params as plain text *)
              let text = { text = params; locale_key = None } in
              Ok (Notification text, advance state) )
      | _ -> Ok (Directive { command; params }, advance state) )
  | Some Lexer.Arrow -> (
      let state = advance state in
      match current_token state with
      | Some (Lexer.Text target) -> Ok (Transition target, advance state)
      | _ -> Error (make_error state "Expected state name after arrow") )
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
  | _ -> Error (make_error state "Expected state marker")

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

(** Helper type for tracking story metadata during parsing *)
type story_info =
  | SceneInfo of {
      background : string option;
      music : string option;
    }
  | NpcInfo of { name : string option }
  | MerchantInfo of {
      name : string option;
      initial_gold : int option;
    }
  | QuestInfo of {
      title : string option;
      description : string option;
      objectives : string list;
      success_description : string option;
      failed_description : string option;
    }
  | NoInfo

(** Parse metadata directives at the beginning of a story *)
let parse_metadata state =
  let rec collect_metadata state story_info tags uses =
    match current_token state with
    | Some (Directive (cmd, params)) -> (
        let state = advance state in
        match cmd with
        | "scene" ->
            collect_metadata state
              (SceneInfo { background = None; music = None })
              tags uses
        | "npc" ->
            let name = if String.is_empty params then None else Some params in
            collect_metadata state (NpcInfo { name }) tags uses
        | "merchant" ->
            let name = if String.is_empty params then None else Some params in
            collect_metadata state (MerchantInfo { name; initial_gold = None }) tags uses
        | "quest" ->
            let title = if String.is_empty params then None else Some params in
            collect_metadata state
              (QuestInfo
                 {
                   title;
                   description = None;
                   objectives = [];
                   success_description = None;
                   failed_description = None;
                 } )
              tags uses
        | "background" -> (
            match story_info with
            | SceneInfo scene ->
                collect_metadata state
                  (SceneInfo { scene with background = Some params })
                  tags uses
            | _ ->
                Error
                  (make_error state
                     (Printf.sprintf
                        "@background can only be used with @scene stories (line %d, \
                         column %d)"
                        state.line state.column ) ) )
        | "music" -> (
            match story_info with
            | SceneInfo scene ->
                collect_metadata state
                  (SceneInfo { scene with music = Some params })
                  tags uses
            | _ ->
                Error
                  (make_error state
                     (Printf.sprintf
                        "@music can only be used with @scene stories (line %d, column %d)"
                        state.line state.column ) ) )
        | "initial_gold" -> (
            match story_info with
            | MerchantInfo merchant -> (
                match Int.of_string_opt params with
                | Some gold ->
                    collect_metadata state
                      (MerchantInfo { merchant with initial_gold = Some gold })
                      tags uses
                | None ->
                    Error
                      (make_error state
                         (Printf.sprintf
                            "Invalid value for @initial_gold: '%s' is not a valid number \
                             (line %d, column %d)"
                            params state.line state.column ) ) )
            | _ ->
                Error
                  (make_error state
                     (Printf.sprintf
                        "@initial_gold can only be used with @merchant stories (line %d, \
                         column %d)"
                        state.line state.column ) ) )
        | "description" -> (
            match story_info with
            | QuestInfo quest ->
                collect_metadata state
                  (QuestInfo { quest with description = Some params })
                  tags uses
            | _ ->
                Error
                  (make_error state
                     (Printf.sprintf
                        "@description can only be used with @quest stories (line %d, \
                         column %d)"
                        state.line state.column ) ) )
        | "objective" -> (
            match story_info with
            | QuestInfo quest ->
                collect_metadata state
                  (QuestInfo { quest with objectives = quest.objectives @ [ params ] })
                  tags uses
            | _ ->
                Error
                  (make_error state
                     (Printf.sprintf
                        "@objective can only be used with @quest stories (line %d, \
                         column %d)"
                        state.line state.column ) ) )
        | "success_description" -> (
            match story_info with
            | QuestInfo quest ->
                collect_metadata state
                  (QuestInfo { quest with success_description = Some params })
                  tags uses
            | _ ->
                Error
                  (make_error state
                     (Printf.sprintf
                        "@success_description can only be used with @quest stories (line \
                         %d, column %d)"
                        state.line state.column ) ) )
        | "failed_description" -> (
            match story_info with
            | QuestInfo quest ->
                collect_metadata state
                  (QuestInfo { quest with failed_description = Some params })
                  tags uses
            | _ ->
                Error
                  (make_error state
                     (Printf.sprintf
                        "@failed_description can only be used with @quest stories (line \
                         %d, column %d)"
                        state.line state.column ) ) )
        | "tags" ->
            let new_tags = String.split params ~on:',' |> List.map ~f:String.strip in
            collect_metadata state story_info (tags @ new_tags) uses
        | "uses" ->
            let new_uses = String.split params ~on:',' |> List.map ~f:String.strip in
            collect_metadata state story_info tags (uses @ new_uses)
        | unknown ->
            Error
              (make_error state
                 (Printf.sprintf "Unknown directive '@%s' at line %d, column %d" unknown
                    state.line state.column ) ) )
    | Some Newline -> collect_metadata (advance state) story_info tags uses
    | _ ->
        (* Return parsed metadata and story_info for validation *)
        Ok (story_info, tags, uses, state)
  in
  collect_metadata state NoInfo [] []

(** Main parse function that converts tokens to an AST.
    @param tokens List of tokens from the lexer
    @param filename Optional filename for error reporting
    @return Either a story AST or a parse error *)
let parse ?(filename = "<unknown>") tokens =
  let state = create_parser tokens in
  match parse_metadata state with
  | Error e -> Error e
  | Ok (story_info, tags, uses, state) -> (
      (* Check if story type is specified *)
      match story_info with
      | NoInfo ->
          Error
            (make_error state
               (Printf.sprintf
                  "Story type not specified in file '%s'. Please add @scene, @npc, \
                   @merchant, or @quest at the beginning of the file (line %d, column \
                   %d)"
                  filename state.line state.column ) )
      | SceneInfo { background; music } -> (
          let metadata = { Ast.story_type = Scene { background; music }; tags; uses } in
          match parse_states state [] with
          | Ok states -> Ok { Ast.metadata; states }
          | Error e -> Error e )
      | NpcInfo { name } -> (
          let localizable_name =
            Option.map name ~f:(fun n -> { text = n; locale_key = None })
          in
          let metadata =
            { Ast.story_type = Npc { name = localizable_name }; tags; uses }
          in
          match parse_states state [] with
          | Ok states -> Ok { Ast.metadata; states }
          | Error e -> Error e )
      | MerchantInfo { name; initial_gold } -> (
          let localizable_name =
            Option.map name ~f:(fun n -> { text = n; locale_key = None })
          in
          let metadata =
            {
              Ast.story_type =
                Merchant { name = localizable_name; inventory = []; initial_gold };
              tags;
              uses;
            }
          in
          match parse_states state [] with
          | Ok states -> Ok { Ast.metadata; states }
          | Error e -> Error e )
      | QuestInfo
          { title; description; objectives; success_description; failed_description } -> (
          let localizable_title =
            Option.map title ~f:(fun t -> { text = t; locale_key = None })
          in
          let localizable_description =
            Option.map description ~f:(fun d -> { text = d; locale_key = None })
          in
          let localizable_objectives =
            List.map objectives ~f:(fun o -> { text = o; locale_key = None })
          in
          let localizable_success =
            Option.map success_description ~f:(fun s -> { text = s; locale_key = None })
          in
          let localizable_failed =
            Option.map failed_description ~f:(fun f -> { text = f; locale_key = None })
          in
          let metadata =
            {
              Ast.story_type =
                Quest
                  {
                    title = localizable_title;
                    description = localizable_description;
                    objectives = localizable_objectives;
                    success_description = localizable_success;
                    failed_description = localizable_failed;
                  };
              tags;
              uses;
            }
          in
          match parse_states state [] with
          | Ok states -> Ok { Ast.metadata; states }
          | Error e -> Error e ) )
