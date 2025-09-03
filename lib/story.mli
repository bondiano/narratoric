(** Story Mode DSL for the Narratoric engine.

    This module provides the complete implementation of Story Mode, the simplest level of
    the Narratoric DSL hierarchy. *)

(** Abstract Syntax Tree definitions *)
module Ast = Ast

(** Lexer for tokenizing Story Mode DSL text *)
module Lexer = Lexer

(** Parser for converting tokens to AST *)
module Parser = Parser
