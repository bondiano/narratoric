(** JavaScript Code Generator for Story Mode DSL - Refactored Version

    This module converts Story Mode AST into optimized JavaScript ES6 modules with a
    minimal runtime system. *)

open Base
open Ast
open Stdio

(** JavaScript AST types for structured code generation *)
module JsAst = struct
  type js_value =
    | JsString of string
    | JsNumber of int
    | JsBool of bool
    | JsObject of (string * js_value) list
    | JsArray of js_value list
    | JsIdentifier of string

  type js_statement =
    | JsImport of {
        from : string;
        imports : string list;
      }
    | JsExport of {
        name : string;
        value : js_value;
      }
end

(** JavaScript code formatter - converts JS AST to formatted code *)
module JsFormatter = struct
  open JsAst

  let indent_size = 2

  let indent level = String.make (level * indent_size) ' '

  let rec format_value ?(level = 0) ?(inline = false) = function
    | JsString s -> Printf.sprintf "\"%s\"" s
    | JsNumber n -> Int.to_string n
    | JsBool b -> Bool.to_string b
    | JsIdentifier name -> name
    | JsArray values ->
        if List.is_empty values then "[]"
        else
          let formatted_values = List.map values ~f:(format_value ~level:(level + 1)) in
          let is_single_object =
            List.exists values ~f:(function JsObject _ -> true | _ -> false)
          in
          if List.length formatted_values = 1 && not is_single_object then
            "[" ^ String.concat ~sep:", " formatted_values ^ "]"
          else
            "[\n"
            ^ String.concat ~sep:",\n"
                (List.map formatted_values ~f:(fun v -> indent (level + 1) ^ v))
            ^ "\n" ^ indent level ^ "]"
    | JsObject pairs ->
        if List.is_empty pairs then "{}"
        else
          let format_pair (key, value) =
            (* Use ES6 shorthand for identical key-value pairs *)
            match value with
            | JsIdentifier name when String.equal key name -> key
            | JsObject nested_pairs ->
                (* For nested objects, format on same line as key *)
                let nested_formatted =
                  format_value ~level:(level + 1) ~inline:true (JsObject nested_pairs)
                in
                Printf.sprintf "%s: %s" key nested_formatted
            | _ -> Printf.sprintf "%s: %s" key (format_value ~level:(level + 1) value)
          in
          let formatted_pairs = List.map pairs ~f:format_pair in
          if inline then
            "{\n"
            ^ String.concat ~sep:",\n"
                (List.map formatted_pairs ~f:(fun p -> indent (level + 1) ^ p))
            ^ "\n" ^ indent level ^ "}"
          else
            "{\n"
            ^ String.concat ~sep:",\n"
                (List.map formatted_pairs ~f:(fun p -> indent (level + 1) ^ p))
            ^ "\n" ^ indent level ^ "}"

  let format_statement = function
    | JsImport { from; imports } ->
        Printf.sprintf "import { %s } from \"%s\";" (String.concat ~sep:", " imports) from
    | JsExport { name; value } ->
        Printf.sprintf "export const %s = %s;" name
          (format_value ~level:0 ~inline:false value)

  let format_program statements =
    String.concat ~sep:"\n\n" (List.map statements ~f:format_statement)
end

(** Block type constants *)
module BlockTypes = struct
  let narration = "narration"

  let dialogue = "dialogue"

  let choice = "choice"

  let conditional = "conditional"

  let skill_check = "skillCheck"

  let variable_set = "variableSet"

  let item_add = "itemAdd"

  let item_remove = "itemRemove"

  let directive = "directive"

  let transition = "transition"
end

(** JavaScript code generation utilities *)
module JsUtils = struct
  (** Escape a string for JavaScript *)
  let escape_js_string s =
    s
    |> String.substr_replace_all ~pattern:"\\" ~with_:"\\\\"
    |> String.substr_replace_all ~pattern:"\"" ~with_:"\\\""
    |> String.substr_replace_all ~pattern:"\n" ~with_:"\\n"
    |> String.substr_replace_all ~pattern:"\r" ~with_:"\\r"
    |> String.substr_replace_all ~pattern:"\t" ~with_:"\\t"

  (** Quote a string for JavaScript *)
  let quote_string s = Printf.sprintf "\"%s\"" (escape_js_string s)

  (** Generate a valid JavaScript identifier *)
  let js_identifier s =
    s
    |> String.substr_replace_all ~pattern:"-" ~with_:"_"
    |> String.substr_replace_all ~pattern:" " ~with_:"_"
    |> String.lowercase

  (** Indent JavaScript code *)
  let indent level content =
    let spaces = String.make (level * 2) ' ' in
    String.split_lines content
    |> List.map ~f:(fun line -> if String.is_empty line then line else spaces ^ line)
    |> String.concat ~sep:"\n"

  (** Generate JavaScript array *)
  let js_array items =
    let items_str = String.concat ~sep:",\n" items in
    Printf.sprintf "[\n%s\n]" (indent 1 items_str)

  (** Generate JavaScript object notation *)
  let js_object pairs =
    let format_pair (key, value) = Printf.sprintf "%s: %s" key value in
    let pairs_str = List.map pairs ~f:format_pair |> String.concat ~sep:",\n" in
    Printf.sprintf "{\n%s\n}" (indent 1 pairs_str)

  (** Helper to create optional pairs for objects *)
  let optional_pair key value_opt transform =
    Option.map value_opt ~f:(fun v -> (key, transform v)) |> Option.to_list

  (** Helper to create type pair *)
  let type_pair block_type = ("type", quote_string block_type)
end

(** Expression compilation to JavaScript *)
module ExpressionGen = struct
  open JsUtils

  (** Compile a literal to JavaScript *)
  let compile_literal = function
    | Int n -> Int.to_string n
    | String s -> quote_string s
    | Bool b -> Bool.to_string b

  (** Compile binary operator to JavaScript *)
  let compile_binary_op = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Eq -> "==="
    | Neq -> "!=="
    | Lt -> "<"
    | Lte -> "<="
    | Gt -> ">"
    | Gte -> ">="
    | And -> "&&"
    | Or -> "||"

  (** Compile unary operator to JavaScript *)
  let compile_unary_op = function Not -> "!" | Neg -> "-"

  (** Compile expression to JavaScript *)
  let rec compile = function
    | Literal lit -> compile_literal lit
    | Variable name -> Printf.sprintf "runtime.getVariable('%s')" (escape_js_string name)
    | BinaryOp { op; left; right } ->
        Printf.sprintf "(%s %s %s)" (compile left) (compile_binary_op op) (compile right)
    | UnaryOp { op; expr } -> Printf.sprintf "%s(%s)" (compile_unary_op op) (compile expr)
    | FunctionCall { name; args } ->
        let args_str = List.map args ~f:compile |> String.concat ~sep:", " in
        Printf.sprintf "runtime.%s(%s)" (js_identifier name) args_str

  (** Compile binary operator to string for array format *)
  let compile_binary_op_str = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Lte -> "<="
    | Gt -> ">"
    | Gte -> ">="
    | And -> "and"
    | Or -> "or"

  (** Compile expression to object AST format for easy runtime interpretation *)
  let rec compile_to_ast_object = function
    | Literal lit ->
        Printf.sprintf "{ type: \"literal\", value: %s }"
          (quote_string (compile_literal lit))
    | Variable name ->
        Printf.sprintf "{ type: \"variable\", name: %s }" (quote_string name)
    | BinaryOp { op; left; right } ->
        let left_ast = compile_to_ast_object left in
        let right_ast = compile_to_ast_object right in
        let op_str = compile_binary_op_str op in
        Printf.sprintf "{ type: \"binary\", operator: %s, left: %s, right: %s }"
          (quote_string op_str) left_ast right_ast
    | UnaryOp { op; expr } ->
        let op_str = compile_unary_op op in
        let expr_ast = compile_to_ast_object expr in
        Printf.sprintf "{ type: \"unary\", operator: %s, operand: %s }"
          (quote_string op_str) expr_ast
    | FunctionCall { name; args } ->
        let args_ast =
          List.map args ~f:compile_to_ast_object |> String.concat ~sep:", "
        in
        Printf.sprintf "{ type: \"function\", name: %s, args: [%s] }"
          (quote_string (js_identifier name))
          args_ast
end

(** AST to JavaScript AST mapper *)
module AstMapper = struct
  open JsAst

  (** Convert expression to JS AST *)
  let rec map_expression = function
    | Literal lit -> (
        match lit with Int n -> JsNumber n | String s -> JsString s | Bool b -> JsBool b )
    | Variable name -> JsObject [ ("type", JsString "variable"); ("name", JsString name) ]
    | BinaryOp { op; left; right } ->
        let op_str =
          match op with
          | Add -> "+"
          | Sub -> "-"
          | Mul -> "*"
          | Div -> "/"
          | Eq -> "=="
          | Neq -> "!="
          | Lt -> "<"
          | Lte -> "<="
          | Gt -> ">"
          | Gte -> ">="
          | And -> "and"
          | Or -> "or"
        in
        JsObject
          [
            ("type", JsString "binary");
            ("operator", JsString op_str);
            ("left", map_expression left);
            ("right", map_expression right);
          ]
    | UnaryOp { op; expr } ->
        let op_str = match op with Not -> "!" | Neg -> "-" in
        JsObject
          [
            ("type", JsString "unary");
            ("operator", JsString op_str);
            ("operand", map_expression expr);
          ]
    | FunctionCall { name; args } ->
        JsObject
          [
            ("type", JsString "function");
            ("name", JsString name);
            ("args", JsArray (List.map args ~f:map_expression));
          ]

  (** Convert block to JS AST *)
  let rec map_block = function
    | Narration text ->
        JsObject [ ("type", JsString "narration"); ("content", JsString text) ]
    | Dialogue { speaker; text } ->
        JsObject
          [
            ("type", JsString "dialogue");
            ("speaker", JsString speaker);
            ("content", JsString text);
          ]
    | Choice { text; target; condition } ->
        let base = [ ("type", JsString "choice"); ("text", JsString text) ] in
        let with_target =
          match target with Some t -> ("target", JsString t) :: base | None -> base
        in
        let with_condition =
          match condition with
          | Some cond -> ("condition", map_expression cond) :: with_target
          | None -> with_target
        in
        JsObject with_condition
    | Conditional { condition; then_blocks; else_blocks } ->
        let base =
          [
            ("type", JsString "conditional");
            ("condition", map_expression condition);
            ("thenBlocks", JsArray (List.map then_blocks ~f:map_block));
          ]
        in
        let with_else =
          match else_blocks with
          | Some blocks -> ("elseBlocks", JsArray (List.map blocks ~f:map_block)) :: base
          | None -> base
        in
        JsObject with_else
    | SkillCheck { description; success_blocks; failure_blocks } ->
        JsObject
          [
            ("type", JsString "skillCheck");
            ("description", JsString description);
            ("successBlocks", JsArray (List.map success_blocks ~f:map_block));
            ("failureBlocks", JsArray (List.map failure_blocks ~f:map_block));
          ]
    | VariableSet { name; value } ->
        JsObject
          [
            ("type", JsString "variableSet");
            ("name", JsString name);
            ("value", map_expression value);
          ]
    | ItemAdd item -> JsObject [ ("type", JsString "itemAdd"); ("item", JsString item) ]
    | ItemRemove item ->
        JsObject [ ("type", JsString "itemRemove"); ("item", JsString item) ]
    | Directive { command; params } ->
        JsObject
          [
            ("type", JsString "directive");
            ("command", JsString command);
            ("params", JsString params);
          ]
    | Transition target ->
        JsObject [ ("type", JsString "transition"); ("target", JsString target) ]

  (** Convert state to JS AST *)
  let map_state { name; blocks } =
    let js_blocks = List.map blocks ~f:map_block in
    JsObject [ ("name", JsString name); ("blocks", JsArray js_blocks) ]

  (** Convert story to complete JS module AST *)
  let map_story story =
    let states_list =
      List.fold story ~init:[] ~f:(fun acc state ->
          let js_identifier =
            String.substr_replace_all state.name ~pattern:"-" ~with_:"_"
            |> String.lowercase
          in
          (js_identifier, map_state state) :: acc )
      |> List.rev
    in
    let states_obj = JsObject states_list in

    let story_obj =
      JsObject [ ("states", states_obj); ("runtime", JsIdentifier "runtime") ]
    in

    [
      JsImport { from = "@narratoric/core"; imports = [ "runtime" ] };
      JsExport { name = "story"; value = story_obj };
    ]
end

(** Block compilation to JavaScript objects *)
module BlockGen = struct
  open JsUtils
  open BlockTypes

  (** Helper to compile blocks array *)
  let compile_blocks_array blocks compile_fn =
    blocks |> List.map ~f:compile_fn |> js_array

  (** Compile simple text block (narration, item operations, etc) *)
  let compile_simple_block block_type key value =
    js_object [ type_pair block_type; (key, quote_string value) ]

  (** Compile narration block *)
  let compile_narration text = compile_simple_block narration "content" text

  (** Compile dialogue block *)
  let compile_dialogue speaker text =
    js_object
      [
        type_pair dialogue;
        ("speaker", quote_string speaker);
        ("content", quote_string text);
      ]

  (** Compile choice block *)
  let compile_choice text target condition =
    let base = [ type_pair choice; ("text", quote_string text) ] in
    let target_pairs = optional_pair "target" target quote_string in
    let condition_pairs =
      optional_pair "condition" condition ExpressionGen.compile_to_ast_object
    in
    js_object (base @ target_pairs @ condition_pairs)

  (** Compile conditional block *)
  let rec compile_conditional condition then_blocks else_blocks =
    let base =
      [
        type_pair conditional;
        ("condition", ExpressionGen.compile_to_ast_object condition);
        ("thenBlocks", compile_blocks_array then_blocks compile_block);
      ]
    in
    let else_pairs =
      optional_pair "elseBlocks" else_blocks (fun blocks ->
          compile_blocks_array blocks compile_block )
    in
    js_object (base @ else_pairs)

  (** Compile skill check block *)
  and compile_skill_check description success_blocks failure_blocks =
    js_object
      [
        type_pair skill_check;
        ("description", quote_string description);
        ("successBlocks", compile_blocks_array success_blocks compile_block);
        ("failureBlocks", compile_blocks_array failure_blocks compile_block);
      ]

  (** Compile variable set block *)
  and compile_variable_set name value =
    js_object
      [
        type_pair variable_set;
        ("name", quote_string name);
        ("value", ExpressionGen.compile value);
      ]

  (** Compile directive block *)
  and compile_directive command params =
    js_object
      [
        type_pair directive;
        ("command", quote_string command);
        ("params", quote_string params);
      ]

  (** Compile transition block *)
  and compile_transition target = compile_simple_block transition "target" target

  (** Compile a single block to JavaScript object *)
  and compile_block = function
    | Narration text -> compile_narration text
    | Dialogue { speaker; text } -> compile_dialogue speaker text
    | Choice { text; target; condition } -> compile_choice text target condition
    | Conditional { condition; then_blocks; else_blocks } ->
        compile_conditional condition then_blocks else_blocks
    | SkillCheck { description; success_blocks; failure_blocks } ->
        compile_skill_check description success_blocks failure_blocks
    | VariableSet { name; value } -> compile_variable_set name value
    | ItemAdd item -> compile_simple_block item_add "item" item
    | ItemRemove item -> compile_simple_block item_remove "item" item
    | Directive { command; params } -> compile_directive command params
    | Transition target -> compile_transition target

  (** Compile list of blocks to JavaScript array *)
  let compile_blocks blocks = compile_blocks_array blocks compile_block
end

(** State compilation *)
module StateGen = struct
  open JsUtils

  (** Compile a state to JavaScript object *)
  let compile_state { name; blocks } =
    let state_obj =
      js_object
        [ ("name", quote_string name); ("blocks", BlockGen.compile_blocks blocks) ]
    in
    (js_identifier name, state_obj)

  (** Compile all states to JavaScript object *)
  let compile_states states = states |> List.map ~f:compile_state |> js_object
end

(** Main code generation *)
module CodeGen = struct
  (** Module template *)
  let module_template ~states_js =
    Printf.sprintf
      {|import { runtime } from "@narratoric/core";

export const story = {
  states: %s,
  runtime
};
|}
      states_js

  (** Generate complete JavaScript module from story *)
  let generate_js_module ?story_id:(_ = "story") story =
    let states_js = StateGen.compile_states story in
    module_template ~states_js

  (** Generate optimized JavaScript (minified version) *)
  let generate_optimized_js ?(story_id = "story") story =
    (* TODO: Implement minification with integer type codes *)
    generate_js_module ~story_id story
end

(** Public API *)

(** Generate JavaScript using new two-stage AST system *)
let compile_to_js_ast ?story_id:(_ = "story") story =
  let js_ast_statements = AstMapper.map_story story in
  JsFormatter.format_program js_ast_statements

(** Generate JavaScript ES6 module from Story AST *)
let compile_to_js ?(story_id = "story") ?(optimize = false) story =
  if optimize then CodeGen.generate_optimized_js ~story_id story
  else compile_to_js_ast ~story_id story

(** Write compiled JavaScript to file *)
let compile_to_file ?(story_id = "story") ?(optimize = false) story output_path =
  let js_code = compile_to_js ~story_id ~optimize story in
  let oc = Out_channel.create output_path in
  Out_channel.output_string oc js_code;
  Out_channel.close oc
