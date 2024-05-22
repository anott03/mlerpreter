structure T = Token

signature AST = sig
  type identifier
  type int_lit
  type bool_lit
  type string_lit

  datatype Expression = EMPTY
                      | IDENTIFIER of identifier
                      | INT_LIT    of int_lit
                      | BOOL_LIT   of bool_lit
                      | STRING_LIT of string_lit
                      (* more to be added ... *)

  type Program
  type Env
  type Object

  type let_statement
  type return_statement
  type expression_statement

  datatype statement = LET_STATEMENT of let_statement
                     | RETURN_STATEMENT of return_statement
                     | EXPRESSION_STATEMENT of expression_statement

  type block_statement

  datatype Node = EXPRESSION      of Expression
                | STATEMENT       of statement
                | BLOCK_STATEMENT of block_statement
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

  type let_statement = { token: T.Token, name: identifier, value: Expression }
  type return_statement = { token: T.Token, value: Expression }
  type expression_statement = { token: T.Token, expression: Expression }

  datatype statement = LET_STATEMENT of let_statement
                     | RETURN_STATEMENT of return_statement
                     | EXPRESSION_STATEMENT of expression_statement
  type block_statement = { token: T.Token, statements: statement list }

  datatype Node = EXPRESSION      of Expression
                | STATEMENT       of statement
                | BLOCK_STATEMENT of block_statement

  type Program = { statements: Node list }

  type Env = (string * Expression) list
  type Object = { value: int }
end
