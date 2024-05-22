structure T = Token
structure A = Ast

datatype 'a Result = SOME of 'a
                   | NONE

signature PARSER = sig
  type Parser
  type Lexer

  val new_parser    : Lexer -> Parser
  val parserString  : Parser -> string
  val next_token    : Parser -> Parser
  val expect_peek   : Parser * T.Token -> bool
  val expect_curr   : Parser * T.Token -> bool
  val peek_error    : Parser * T.Token -> Parser
  val peek_priority : Parser -> int
  val get_priority  : T.Token -> int

  val parse_program : Parser * A.Program -> A.Program
end

functor ParserNew(structure L: LEXER) : PARSER = struct
  type Parser = { lexer:      L.Lexer,
                  curr_token: T.Token,
                  peek_token: T.Token,
                  errors:     string list }
  type Lexer  = L.Lexer

  fun parserString parser =
    let val { curr_token=curr_token,
              peek_token=peek_token,
              errors=errors, ... } = parser
    in
      "{ curr_token=" ^ (T.tokenString curr_token) ^ ", peek_token="
      ^ (T.tokenString peek_token) ^ " }"
    end

  fun new_parser l =
    let val (l, firstToken)     = L.next_token l
        val (l, secondToken)    = L.next_token l
        val errors: string list = []
    in
      { curr_token=firstToken, peek_token=secondToken, errors=errors, lexer=l }
    end

  fun next_token p =
    let val { lexer=lexer, peek_token=peek_token, errors=errors, ... } = p
        val (lexer, nt) = L.next_token lexer
    in
      { lexer=lexer, curr_token=peek_token, peek_token=nt, errors=errors } 
    end

  fun expect_peek (p, t) =
    let val { peek_token, ... } = p
    in
      t = peek_token
    end

  fun expect_curr (p, t) =
    let val { curr_token, ... } = p
    in
      t = curr_token
    end

  fun peek_error (p, t) = 
    let val { peek_token, errors, ... } = p
        val err_msg = "Expected " ^ (T.tokenString t) ^ " but got "
                                                    ^ (T.tokenString peek_token)
        val errors  = errors @ [err_msg]
    in
      { lexer=(#lexer p), curr_token=(#curr_token p), peek_token=peek_token,
                                                                 errors=errors }
    end

  fun get_priority T.EQ       = 1
    | get_priority T.NEQ      = 1
    | get_priority T.LT       = 2
    | get_priority T.GT       = 2
    | get_priority T.PLUS     = 3
    | get_priority T.MINUS    = 3
    | get_priority T.SLASH    = 4
    | get_priority T.ASTERISK = 4
    | get_priority T.LPAREN   = 5
    | get_priority _          = 0

  fun peek_priority p =
    let val { peek_token, ... } = p
    in
      get_priority peek_token
    end

  (* val parse_return_statement : Parser * A.Program -> A.statement *)
  fun parse_return_statement (p, prog) =
              A.EXPRESSION_STATEMENT { token = T.ILLEGAL, expression = A.EMPTY }

  (* val parse_expression_statement : Parser * A.Program -> A.statement *)
  fun parse_expression_statement (p, prog) =
              A.EXPRESSION_STATEMENT { token = T.ILLEGAL, expression = A.EMPTY }

  (* val parse_expression : Parser * int -> A.Expression *)
  fun parse_expression (p, priority) = A.EMPTY

  (* val parse_let_statement : Parser * A.Program -> A.statement *)
  fun parse_let_statement (p, prog) =
    let val { curr_token, ... } = p
        (* assert that the next token is an ident *)
        (* TODO *)
        val { curr_token, ... } = next_token p
        val ident = { token=curr_token, value=(T.get_literal curr_token) }
        (* assert that the next token is assign *)
        (* TODO *)
        val p = next_token (next_token p)
        val value = parse_expression (p, 0)
        (* assert that the next token is a semicolon *)
        (* TODO *)
    in
      A.LET_STATEMENT { token=curr_token, name=ident, value=value }
    end

  (* val parse_statement : Parser * A.Program -> A.statement *)
  fun parse_statement (p, prog) =
    let val { curr_token, ... } = p
    in case curr_token
         of T.LET    => parse_let_statement (p, prog)
          | T.RETURN => parse_return_statement (p, prog)
          | _      => parse_expression_statement (p, prog)
    end

  (*
  * parse_program ({ curr_token=EOF, ... }, prog) = prog
  * parse_program (p, prog) =
                         parse_program (next_token p, parse_statement (p, prog))
  *)
  fun parse_program (p, prog) =
    let val { curr_token, ... } = p
        val { statements } = prog
        val statement = parse_statement (p, prog)
        val prog = { statements=(statements @ [statement]) }
    in if curr_token = T.EOF
         then prog
         else parse_program (next_token p, prog)
    end
end
