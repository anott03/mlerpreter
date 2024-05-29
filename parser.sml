structure T = Token
structure A = Ast

datatype 'a Result = SOME of 'a
                   | NONE

signature PARSER = sig
  type Parser
  type Lexer

  exception PeekError of string

  val new_parser    : Lexer -> Parser
  val parse_program : Parser * A.Program -> A.Program
end

functor ParserNew(structure L: LEXER) : PARSER = struct
  type Parser = { lexer:      L.Lexer,
                  curr_token: T.Token,
                  peek_token: T.Token,
                  errors:     string list }
  type Lexer  = L.Lexer

  exception PeekError of string

  fun parserString parser =
    let val { curr_token, peek_token, ... }: Parser = parser
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
    let val { lexer, peek_token, errors, ... }: Parser = p
        val (lexer, nt) = L.next_token lexer
    in
      { lexer=lexer, curr_token=peek_token, peek_token=nt, errors=errors } 
    end

  fun expect_peek (p, T.IDENT _) =
    let val { peek_token, ... }: Parser = p
    in case peek_token
         of T.IDENT _ => true
          | _         => false
    end
    | expect_peek (p, T.INT _) =
    let val { peek_token, ... }: Parser = p
    in case peek_token
         of T.INT _ => true
          | _       => false
    end
    | expect_peek (p, T.STRING _) =
    let val { peek_token, ... }: Parser = p
    in case peek_token
         of T.STRING _ => true
          | _          => false
    end
    | expect_peek (p, t) =
      let val { peek_token, ... }: Parser = p
      in
        t = peek_token
      end

  fun expect_curr (p, T.IDENT _) =
    let val { curr_token, ... }: Parser = p
    in case curr_token
         of T.IDENT _ => true
          | _         => false
    end
    | expect_curr (p, T.INT _) =
    let val { curr_token, ... }: Parser = p
    in case curr_token
         of T.INT _ => true
          | _       => false
    end
    | expect_curr (p, T.STRING _) =
    let val { curr_token, ... }: Parser = p
    in case curr_token
         of T.STRING _ => true
          | _          => false
    end
    | expect_curr (p, t) =
      let val { curr_token, ... }: Parser = p
      in
        t = curr_token
      end

  fun peek_error (p, t) = 
    let val { lexer=lexer, curr_token=curr_token, peek_token=peek_token, errors=errors } = p
        val err_msg = "Expected " ^ (T.tokenString t) ^ " but got "
                                                    ^ (T.tokenString peek_token)
        val errors  = errors @ [err_msg]
    in
      { lexer=lexer, curr_token=curr_token, peek_token=peek_token,
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
    let val { peek_token, ... }: Parser = p
    in
      get_priority peek_token
    end

  (* val parse_expression : Parser * int -> Parser * A.Expression *)
  fun parse_expression (p, priority) = (p, A.EMPTY) (* TODO *)

  (* val parse_return_statement : Parser * A.Program -> A.statement *)
  fun parse_return_statement (p, prog) = (* TODO *)
              A.EXPRESSION_STATEMENT { token = T.ILLEGAL, expression = A.EMPTY }

  (* val parse_expression_statement : Parser * A.Program -> A.statement *)
  fun parse_expression_statement (p, prog) = (* TODO *)
              A.EXPRESSION_STATEMENT { token = T.ILLEGAL, expression = A.EMPTY }

  (* val parse_let_statement : Parser * A.Program -> A.statement *)
  fun parse_let_statement p =
    let val { peek_token, ... } = p
        val peek_valid = expect_peek (p, T.IDENT "")
        val peek_msg =
          if peek_valid then ""
          else "expected IDENT(...) but got " ^ (T.tokenString peek_token)

        val p = next_token p
        val { curr_token, ... } = p
        val ident = { token=curr_token, value=(T.get_literal curr_token) }

        val peek_valid = expect_peek (p, T.ASSIGN)
        val peek_msg = if not peek_valid andalso peek_msg = "" then
                         "expected ASSIGN but got " ^ (T.tokenString peek_token)
                       else
                         peek_msg

        val p = next_token (next_token p)
        val (p, value) = parse_expression (p, 0)

        val peek_valid = expect_peek (p, T.IDENT "")
        val peek_msg = if not peek_valid andalso peek_msg = "" then
                         "expected SEMICOLON but got " ^
                         (T.tokenString peek_token)
                       else
                         peek_msg
    in
      if peek_valid then
        A.LET_STATEMENT { token=curr_token, name=ident, value=value }
      else
        raise PeekError peek_msg
    end

  (* val parse_statement : Parser * A.Program -> A.Node *)
  fun parse_statement (p, prog) =
    let val { curr_token, ... } = p
    in case curr_token
         of T.LET    => A.STATEMENT (parse_let_statement p)
          | T.RETURN => A.STATEMENT (parse_return_statement (p, prog))
          | _        => A.STATEMENT (parse_expression_statement (p, prog))
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
