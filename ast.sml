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

  val statementString : statement -> string
  val nodeString : Node -> string
  val programString : Program -> string
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

  fun intString n =
    String.map (fn #"~" => #"-" | c => c) (Int.toString n)

  fun boolString b = if b then "true" else "false"

  fun expressionString EMPTY = "EMPTY"
    | expressionString (IDENTIFIER ident) =
      let val { token, value } = ident
      in "IDENTIFIER({ " ^ (T.tokenString token) ^ ", value=\"" ^ value ^
         "\" })" end
    | expressionString (INT_LIT il) =
      let val { token, value } = il
      in "INT_LIT({ " ^ (T.tokenString token) ^ ", value=" ^ (intString value) ^
         " })"
      end
    | expressionString (BOOL_LIT il) =
      let val { token, value } = il
      in "BOOL_LIT({ " ^ (T.tokenString token) ^ ", value=" ^ (boolString value)
         ^ " })"
      end
    | expressionString (STRING_LIT ident) =
      let val { token, value } = ident
      in "STRING_LIT({ " ^ (T.tokenString token) ^ ", value=\"" ^ value ^
         "\" })" end

  fun statementString (LET_STATEMENT _) = "LET_STATEMENT"
    | statementString (RETURN_STATEMENT _) = "RETURN_STATEMENT"
    | statementString (EXPRESSION_STATEMENT es) =
      let val { token, expression } = es
      in
        "EXPRESSION_STATEMENT({ token=" ^ (T.tokenString token) ^
        ", expression=" ^ (expressionString expression) ^ " })"
      end

  fun nodeString (EXPRESSION _) = "EXPRESSION"
    | nodeString (STATEMENT s) = statementString s
    | nodeString (BLOCK_STATEMENT _) = "BLOCK_STATEMENT"

  fun programString p =
    let val { statements } = p
        fun statementsString [] = ""
          | statementsString (s::ss) =
                                (nodeString s) ^ "\n" ^ statementsString ss
    in
      statementsString statements
    end
end
