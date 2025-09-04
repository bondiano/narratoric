(** JavaScript Code Generator for Story Mode DSL.

    This module converts Story Mode AST into optimized JavaScript ES6 modules with a
    minimal runtime system. *)

(** Generate JavaScript ES6 module from Story AST.

    @param story_id Optional story identifier (default: "story")
    @param optimize Whether to generate optimized/minified code (default: false)
    @param story The story AST to compile
    @return JavaScript ES6 module as string *)
val compile_to_js : ?story_id:string -> ?optimize:bool -> Ast.story -> string

(** Write compiled JavaScript to file.

    @param story_id Optional story identifier (default: "story")
    @param optimize Whether to generate optimized/minified code (default: false)
    @param story The story AST to compile
    @param output_path Path where to write the JavaScript file *)
val compile_to_file : ?story_id:string -> ?optimize:bool -> Ast.story -> string -> unit
