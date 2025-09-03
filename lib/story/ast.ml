open Base

(** Abstract Syntax Tree (AST) for Story Mode DSL.

    This module defines the AST structure for the Story Mode language, which is the
    simplest level of the Narratoric DSL hierarchy. *)

(** A story is a collection of states/scenes *)
type story = state list

(** A state represents a scene or location in the story *)
and state = {
  name : string;  (** State identifier like "tavern_entrance" *)
  blocks : block list;  (** Content blocks within the state *)
}

(** Different types of blocks that can appear in a state:
    - Narration: Plain text narration
    - Dialogue: Character dialogue with speaker and text ("Character: text")
    - Choice: Player choice with optional target state
    - Conditional: Conditional block with if/else logic ([if condition] ...)
    - SkillCheck: Skill check with success/failure outcomes (? skill check DC 15)
    - VariableSet: Variable assignment ($var = value)
    - ItemAdd: Add item to inventory (+item)
    - ItemRemove: Remove item from inventory (-item)
    - Directive: Engine directive (@command params)
    - Transition: State transition (-> state_name) *)
and block =
  | Narration of string
  | Dialogue of {
      speaker : string;
      text : string;
    }
  | Choice of choice
  | Conditional of conditional
  | SkillCheck of skill_check
  | VariableSet of {
      name : string;
      value : expression;
    }
  | ItemAdd of string
  | ItemRemove of string
  | Directive of {
      command : string;
      params : string;
    }
  | Transition of string

(** Literal value types:
    - Int: Integer literal
    - String: String literal
    - Bool: Boolean literal (true/false) *)
and literal =
  | Int of int
  | String of string
  | Bool of bool

(** Binary operators for expressions:
    - Add: Addition (+)
    - Sub: Subtraction (-)
    - Mul: Multiplication
    - Div: Division (/)
    - Eq: Equality (==)
    - Neq: Inequality (!=)
    - Lt: Less than (<)
    - Lte: Less than or equal (<=)
    - Gt: Greater than (>)
    - Gte: Greater than or equal (>=)
    - And: Logical AND (&&)
    - Or: Logical OR (||) *)
and binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte
  | And
  | Or

(** Unary operators for expressions:
    - Not: Logical NOT (!)
    - Neg: Numeric negation (-) *)
and unary_op =
  | Not
  | Neg

(** Expression types for conditions and computations:
    - Literal: Literal value (int, string, bool)
    - Variable: Variable reference ($variable_name)
    - BinaryOp: Binary operation with operator and two operands
    - UnaryOp: Unary operation with operator and one operand
    - FunctionCall: Function call with name and arguments *)
and expression =
  | Literal of literal
  | Variable of string
  | BinaryOp of {
      op : binary_op;
      left : expression;
      right : expression;
    }
  | UnaryOp of {
      op : unary_op;
      expr : expression;
    }
  | FunctionCall of {
      name : string;
      args : expression list;
    }

(** Player choice structure *)
and choice = {
  text : string;  (** Display text shown to player *)
  target : string option;  (** Optional target state after selection *)
  condition : expression option;  (** Optional condition for availability *)
}

(** Conditional block for branching logic *)
and conditional = {
  condition : expression;  (** Condition to evaluate *)
  then_blocks : block list;  (** Blocks to execute if condition is true *)
  else_blocks : block list option;  (** Optional blocks for false condition *)
}

(** Skill check with success/failure outcomes *)
and skill_check = {
  description : string;  (** Check description, e.g. "perception check DC 15" *)
  success_blocks : block list;  (** Blocks to execute on success (=>) *)
  failure_blocks : block list;  (** Blocks to execute on failure (=|) *)
}

(** Convert an expression to a human-readable string representation *)
let rec show_expression = function
  | Literal (Int n) -> Int.to_string n
  | Literal (String s) -> Printf.sprintf "\"%s\"" s
  | Literal (Bool b) -> Bool.to_string b
  | Variable v -> "$" ^ v
  | BinaryOp { op; left; right } ->
      Printf.sprintf "(%s %s %s)" (show_expression left) (show_binary_op op)
        (show_expression right)
  | UnaryOp { op; expr } ->
      Printf.sprintf "%s%s" (show_unary_op op) (show_expression expr)
  | FunctionCall { name; args } ->
      Printf.sprintf "%s(%s)" name
        (String.concat ~sep:", " (List.map args ~f:show_expression))

and show_binary_op = function
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
  | And -> "&&"
  | Or -> "||"

and show_unary_op = function Not -> "!" | Neg -> "-"
