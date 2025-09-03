open Base

(* Token types for Story Mode lexer *)
type token =
  | Text of string (* Regular narration text *)
  | Choice of string (* * [choice text] *)
  | Arrow (* -> *)
  | StateMarker of string (* ## state_name *)
  | Directive of string * string (* @command params *)
  | Condition of string (* condition text inside [] *)
  | SkillCheck (* ? *)
  | SuccessArrow (* => *)
  | FailureArrow (* =| *)
  | VariableSet of string * string (* $var = value *)
  | AddItem of string (* +item *)
  | RemoveItem of string (* -item *)
  | Speaker of string (* Character: *)
  | Newline (* Line break *)
  | Comment of string (* Comment text - usually skipped *)
  | Eof (* End of file *)

(* Lexer state *)
type lexer_state = {
  input : string;
  position : int;
  length : int;
}

let create_lexer input = { input; position = 0; length = String.length input }

(* Helper functions *)
let is_at_end state = state.position >= state.length

let current_char state =
  if is_at_end state then None else Some (String.get state.input state.position)

let peek_char state offset =
  let pos = state.position + offset in
  if pos >= state.length || pos < 0 then None else Some (String.get state.input pos)

let advance state = { state with position = state.position + 1 }

let advance_by state n = { state with position = state.position + n }

(* Check if string starts with pattern at current position *)
let starts_with state pattern =
  let pattern_len = String.length pattern in
  state.position + pattern_len <= state.length
  && String.equal (String.sub state.input ~pos:state.position ~len:pattern_len) pattern

(* Skip whitespace but not newlines *)
let rec skip_spaces state =
  match current_char state with
  | Some (' ' | '\t' | '\r') -> skip_spaces (advance state)
  | _ -> state

(* Skip OCaml-style comments *)
let rec skip_comment state depth =
  if is_at_end state then state
  else if depth = 0 then state
  else
    match (current_char state, peek_char state 1) with
    | Some '(', Some '*' ->
        (* Start of nested comment *)
        skip_comment (advance_by state 2) (depth + 1)
    | Some '*', Some ')' ->
        (* End of comment *)
        skip_comment (advance_by state 2) (depth - 1)
    | _ -> skip_comment (advance state) depth

(* Check if we're at the start of a comment and skip it *)
let skip_comments state =
  match (current_char state, peek_char state 1) with
  | Some '(', Some '*' -> skip_comment (advance_by state 2) 1
  | _ -> state

(* Skip spaces and comments *)
let rec skip_whitespace_and_comments state =
  let state = skip_spaces state in
  let new_state = skip_comments state in
  if state.position <> new_state.position then skip_whitespace_and_comments new_state
  else state

(* Read until a specific character or end of line *)
let read_until state stop_chars =
  let start_pos = state.position in
  let rec loop state =
    match current_char state with
    | None | Some '\n' -> state
    | Some c when List.mem stop_chars c ~equal:Char.equal -> state
    | _ -> loop (advance state)
  in
  let end_state = loop state in
  let text =
    String.sub state.input ~pos:start_pos ~len:(end_state.position - start_pos)
  in
  (String.strip text, end_state)

(* Read until end of line, stripping trailing comments *)
let read_line state =
  let start_pos = state.position in
  let rec loop state =
    match current_char state with None | Some '\n' -> state | _ -> loop (advance state)
  in
  let end_state = loop state in
  let text =
    String.sub state.input ~pos:start_pos ~len:(end_state.position - start_pos)
  in
  (* Strip trailing comments *)
  let text =
    match String.index text '(' with
    | None -> text
    | Some idx
      when idx + 1 < String.length text && Char.equal (String.get text (idx + 1)) '*' ->
        String.sub text ~pos:0 ~len:idx
    | _ -> text
  in
  (String.strip text, end_state)

(* Lex a single token *)
let rec lex_token state =
  let state = skip_whitespace_and_comments state in

  if is_at_end state then (Eof, state)
  else
    match current_char state with
    | None -> (Eof, state)
    | Some '\n' -> (Newline, advance state)
    (* State marker: ## *)
    | Some '#' when Option.equal Char.equal (peek_char state 1) (Some '#') ->
        let state = advance_by state 2 in
        let state = skip_spaces state in
        let name, state = read_line state in
        (StateMarker name, state)
    (* Choice: * [...] *)
    | Some '*' when Option.equal Char.equal (peek_char state 1) (Some ' ') ->
        let state = advance_by state 2 in
        let state = skip_spaces state in
        (* Check for [ bracket *)
        if Option.equal Char.equal (current_char state) (Some '[') then
          let state = advance state in
          let choice_text, state = read_until state [ ']' ] in
          let state =
            if Option.equal Char.equal (current_char state) (Some ']') then advance state
            else state
          in
          (Choice choice_text, state)
        else
          (* Not a properly formatted choice, treat as text *)
          let text, state = read_line state in
          (Text ("* " ^ text), state)
    (* Arrow: -> *)
    | Some '-' when Option.equal Char.equal (peek_char state 1) (Some '>') ->
        (Arrow, advance_by state 2)
    (* Success arrow: => *)
    | Some '=' when Option.equal Char.equal (peek_char state 1) (Some '>') ->
        (SuccessArrow, advance_by state 2)
    (* Failure arrow: =| *)
    | Some '=' when Option.equal Char.equal (peek_char state 1) (Some '|') ->
        (FailureArrow, advance_by state 2)
    (* Directive: @ *)
    | Some '@' ->
        let state = advance state in
        (* Read command name *)
        let start_pos = state.position in
        let rec read_cmd state =
          match current_char state with
          | Some c when Char.is_alphanum c || Char.equal c '_' -> read_cmd (advance state)
          | _ -> state
        in
        let cmd_end = read_cmd state in
        let cmd =
          String.sub state.input ~pos:start_pos ~len:(cmd_end.position - start_pos)
        in
        let state = skip_spaces cmd_end in
        (* Read parameters until end of line *)
        let params, state = read_line state in
        (Directive (cmd, params), state)
    (* Variable set: $var = *)
    | Some '$' ->
        let state = advance state in
        let start_pos = state.position in
        (* Read variable name *)
        let rec read_var state =
          match current_char state with
          | Some c when Char.is_alphanum c || Char.equal c '_' -> read_var (advance state)
          | _ -> state
        in
        let var_end = read_var state in
        let var_name =
          String.sub state.input ~pos:start_pos ~len:(var_end.position - start_pos)
        in
        let state = skip_spaces var_end in
        (* Check for = *)
        if Option.equal Char.equal (current_char state) (Some '=') then
          let state = advance state in
          let state = skip_spaces state in
          let value, state = read_line state in
          (VariableSet (var_name, value), state)
        else
          (* Not a variable set, treat as text *)
          let text, state = read_line var_end in
          (Text ("$" ^ var_name ^ " " ^ text), state)
    (* Add item: + *)
    | Some '+' when not (Option.equal Char.equal (peek_char state 1) (Some ' ')) ->
        let state = advance state in
        let item, state = read_until state [ ' '; '\n'; '\t' ] in
        (AddItem item, state)
    (* Remove item: - *)
    | Some '-'
      when (not (Option.equal Char.equal (peek_char state 1) (Some '>')))
           && not (Option.equal Char.equal (peek_char state 1) (Some ' ')) ->
        let state = advance state in
        let item, state = read_until state [ ' '; '\n'; '\t' ] in
        (RemoveItem item, state)
    (* Skill check: ? *)
    | Some '?' -> (SkillCheck, advance state)
    (* Condition: [...] *)
    | Some '[' ->
        let state = advance state in
        let cond, state = read_until state [ ']' ] in
        let state =
          if Option.equal Char.equal (current_char state) (Some ']') then advance state
          else state
        in
        (Condition cond, state)
    (* Check for speaker: Name: *)
    | Some _ -> (
        let start_pos = state.position in
        (* Try to find a colon on the same line *)
        let rec find_colon pos =
          if pos >= state.length then None
          else
            match String.get state.input pos with
            | '\n' -> None
            | ':' ->
                (* Check if this looks like a speaker *)
                let potential_speaker =
                  String.sub state.input ~pos:start_pos ~len:(pos - start_pos)
                in
                let trimmed = String.strip potential_speaker in
                (* Simple heuristic: speaker names are usually one or two words *)
                if
                  String.length trimmed > 0
                  && String.length trimmed < 50
                  && (not (String.contains trimmed '['))
                  && (not (String.contains trimmed '$'))
                  && not (String.contains trimmed '@')
                then Some (trimmed, pos + 1)
                else None
            | _ -> find_colon (pos + 1)
        in

        match find_colon state.position with
        | Some (speaker, new_pos) -> (Speaker speaker, { state with position = new_pos })
        | None ->
            (* Regular text *)
            let text, state = read_line state in
            if String.length text > 0 then (Text text, state) else lex_token state )

(* Main lexer function *)
let lex_story content =
  let rec lex_all state acc =
    if is_at_end state then List.rev (Eof :: acc)
    else
      let token, new_state = lex_token state in
      match token with
      | Eof -> List.rev (Eof :: acc)
      | _ -> lex_all new_state (token :: acc)
  in
  lex_all (create_lexer content) []
