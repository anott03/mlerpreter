(* LIBRARY CODE *)
fun println s = (print s; print "\n")
fun eprint   s = TextIO.output (TextIO.stdErr, s)
fun eprintln s = (eprint s; eprint "\n")

(* STRING UTILS *)
fun intString n =
  String.map (fn #"~" => #"-" | c => c) (Int.toString n)

(* PAIR UTILS *)
fun fst (x, _) = x
fun snd (_, y) = y
fun pair x y = (x, y)
fun curry  f x y   = f (x, y)
fun curry3 f x y z = f (x, y, z)

(* TOKEN *)
(* {{{ *)
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
               (* }}} *)

(* val lookup_ident: string -> Token *)
(* {{{ *)
fun lookup_ident "let"    = LET
  | lookup_ident "fn"     = FUNCTION
  | lookup_ident "true"   = TRUE
  | lookup_ident "false"  = FALSE
  | lookup_ident "if"     = IF
  | lookup_ident "else"   = ELSE
  | lookup_ident "return" = RETURN
  | lookup_ident "import" = IMPORT
  | lookup_ident s        = IDENT s
  (* }}} *)

(* val get_literal: Token -> string *)
(* {{{ *)
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
  (* }}} *)

(* for debugging *)
(* val tokenString : Token -> string *)
(* {{{ *)
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
  (* }}} *)

(* LEXER *)
(* The goal is to take the input string and parse it into a series of Tokens *)
type Lexer = {
  input: string,
  ch: char
}

(* val next_token: Lexer -> Lexer * Token *)
fun next_token l =
  let fun nt #"\^Z" = (l, EOF)
        | nt #"," = (l, COMMA)
        | nt #";" = (l, SEMICOLON)
        | nt #"(" = (l, LPAREN)
        | nt #")" = (l, RPAREN)
        | nt #"{" = (l, LSQUIRLY)
        | nt #"}" = (l, RSQUIRLY)
        | nt #"[" = (l, LBRACKET)
        | nt #"]" = (l, RBRACKET)

        | nt #"+" = (l, PLUS)
        | nt #"-" = (l, MINUS)
        | nt #"/" = (l, SLASH)
        | nt #"*" = (l, ASTERISK)
        | nt #"<" = (l, LT)
        | nt #">" = (l, GT)
        | nt ch  = (l, ILLEGAL)

      val { input=inp, ch=c, ... }: Lexer = l
  in
    (nt c)
  end

(* val read_char: Lexer -> Lexer * char *)
fun read_char l =
  let val { input=input, ch=ch } = l
      val firstChar = String.sub(input, 0)
      val remainder = String.extract(input, 1, NONE)
      val newLexer  = { input=remainder, ch = firstChar }
  in
    (newLexer, firstChar)
  end

val lexer = { input = "val x = 10;", ch = #"," }
val (l, ch) = read_char lexer
val () = println (String.str ch)

(*
val (l, t) = next_token lexer
val () = println (tokenString t)
*)
