signature TOKEN = sig
  datatype Token = IDENT of string
                 | INT of string
                 | STRING of string
                 | FUNCTION
                 | LET
                 | TRUE
                 | FALSE
                 | IF
                 | ELSE
                 | RETURN
                 | IMPORT
                 | ILLEGAL
                 | EOF
                 | COMMA
                 | SEMICOLON
                 (* brackets *)
                 | LPAREN
                 | RPAREN
                 | LSQUIRLY
                 | RSQUIRLY
                 | LBRACKET
                 | RBRACKET
                 (* operations *)
                 | ASSIGN
                 | PLUS
                 | MINUS
                 | BANG
                 | ASTERISK
                 | SLASH
                 | POWER
                 (* comparison *)
                 | LT
                 | GT
                 | EQ
                 | NEQ

  val get_literal  : Token -> string
  val lookup_ident: string -> Token
  val get_priority : Token -> int
  val tokenString  : Token -> string
end

structure Token :> TOKEN =
struct
  datatype Token = IDENT of string
                 | INT of string
                 | STRING of string
                 | FUNCTION
                 | LET
                 | TRUE
                 | FALSE
                 | IF
                 | ELSE
                 | RETURN
                 | IMPORT
                 | ILLEGAL
                 | EOF
                 | COMMA
                 | SEMICOLON
                 (* brackets *)
                 | LPAREN
                 | RPAREN
                 | LSQUIRLY
                 | RSQUIRLY
                 | LBRACKET
                 | RBRACKET
                 (* operations *)
                 | ASSIGN
                 | PLUS
                 | MINUS
                 | BANG
                 | ASTERISK
                 | SLASH
                 | POWER
                 (* comparison *)
                 | LT
                 | GT
                 | EQ
                 | NEQ

  fun get_literal (IDENT s)  = s
    | get_literal (INT s)    = s
    | get_literal (STRING s) = s
    | get_literal FUNCTION   = "fn"
    | get_literal LET        = "let"
    | get_literal TRUE       = "true"
    | get_literal FALSE      = "false"
    | get_literal IF         = "if"
    | get_literal ELSE       = "else"
    | get_literal RETURN     = "return"
    | get_literal IMPORT     = "import"
    | get_literal ILLEGAL    = "ILLEGAL"
    | get_literal EOF        = String.str (Char.chr 26) (* \0 *)
    | get_literal COMMA      = ","
    | get_literal SEMICOLON  = ";"
    | get_literal LPAREN     = "("
    | get_literal RPAREN     = ")"
    | get_literal LSQUIRLY   = "{"
    | get_literal RSQUIRLY   = "}"
    | get_literal LBRACKET   = "["
    | get_literal RBRACKET   = "]"
    | get_literal ASSIGN     = "="
    | get_literal PLUS       = "+"
    | get_literal MINUS      = "-"
    | get_literal BANG       = "!"
    | get_literal ASTERISK   = "*"
    | get_literal SLASH      = "/"
    | get_literal LT         = "<"
    | get_literal GT         = ">"
    | get_literal EQ         = "=="
    | get_literal NEQ        = "!="
    | get_literal POWER      = "**"

  fun lookup_ident "let"    = LET
    | lookup_ident "fn"     = FUNCTION
    | lookup_ident "true"   = TRUE
    | lookup_ident "false"  = FALSE
    | lookup_ident "if"     = IF
    | lookup_ident "else"   = ELSE
    | lookup_ident "return" = RETURN
    | lookup_ident "import" = IMPORT
    | lookup_ident s        = IDENT s

  fun get_priority EQ        = 1
    | get_priority NEQ       = 1
    | get_priority LT        = 2
    | get_priority GT        = 2
    | get_priority PLUS      = 3
    | get_priority MINUS     = 3
    | get_priority SLASH     = 4
    | get_priority ASTERISK  = 4
    | get_priority LPAREN    = 5
    | get_priority t         = 0

  fun tokenString (IDENT s)  = "IDENT(" ^ s ^ ")"
    | tokenString (INT s)    = "INT(" ^ s ^ ")"
    | tokenString (STRING s) = "STRING(\"" ^ s ^ "\")"
    | tokenString FUNCTION   = "FUNCTION"
    | tokenString LET        = "LET"
    | tokenString TRUE       = "TRUE"
    | tokenString FALSE      = "FALSE"
    | tokenString IF         = "IF"
    | tokenString ELSE       = "ELSE"
    | tokenString RETURN     = "RETURN"
    | tokenString IMPORT     = "IMPORT"
    | tokenString ILLEGAL    = "ILLEGAL"
    | tokenString EOF        = "EOF"
    | tokenString COMMA      = "COMMA"
    | tokenString SEMICOLON  = "SEMICOLON"
    | tokenString LPAREN     = "LPAREN"
    | tokenString RPAREN     = "RPAREN"
    | tokenString LSQUIRLY   = "LSQUIRLY"
    | tokenString RSQUIRLY   = "RSQUIRLY"
    | tokenString LBRACKET   = "LBRACKET"
    | tokenString RBRACKET   = "RBRACKET"
    | tokenString ASSIGN     = "ASSIGN"
    | tokenString PLUS       = "PLUS"
    | tokenString MINUS      = "MINUS"
    | tokenString BANG       = "BANG"
    | tokenString ASTERISK   = "ASTERISK"
    | tokenString SLASH      = "SLASH"
    | tokenString LT         = "LT"
    | tokenString GT         = "GT"
    | tokenString EQ         = "EQ"
    | tokenString NEQ        = "NEQ"
    | tokenString POWER      = "POWER"
end
