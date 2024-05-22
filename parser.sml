structure T = Token

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
end
