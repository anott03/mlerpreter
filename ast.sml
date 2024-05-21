structure T = Token

signature AST = sig
  type Expression
  type Node
  type Env
  type Object

  (* val eval : Node * Env -> Object *)
end

structure Ast : AST = struct
  type identifier = { token: T.Token, value: string }
  type int_lit    = { token: T.Token, value: int }
  type bool_lit   = { token: T.Token, value: bool }
  type string_lit = { token: T.Token, value: string }

  datatype Expression = EMPTY
                       | IDENTIFIER of identifier
                       | INT_LIT    of int_lit
                       | BOOL_LIT   of bool_lit
                       | STRING_LIT of string_lit
                       (* more to be added ... *)

  datatype Node = PROGRAM         of {}
                | EXPRESSION      of Expression
                | STATEMENT       of {}
                | BLOCK_STATEMENT of {}

  type Env = (string * Expression) list
  type Object = { value: int }

  (*
  fun eval (PROGRAM p, env)          = eval_program (p, env)
    | eval (EXPRESSION ex, env)      = eval_expression (ex, env)
    | eval (STATEMENT s, env)        = eval_statement (s, env)
    | eval (BLOCK_STATEMENT bs, env) = eval_block_statement (bs, env)
    *)
end
