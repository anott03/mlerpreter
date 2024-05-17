(* LIBRARY CODE *)
fun println s = (print s; print "\n")
fun eprint   s = TextIO.output (TextIO.stdErr, s)
fun eprintln s = (eprint s; eprint "\n")

(* STRING UTILS *)
fun intString n =
  String.map (fn #"~" => #"-" | c => c) (Int.toString n)

fun lexerString { input=input, ch=ch } =
  "{ input=\"" ^ input ^ "\", ch=#\"" ^ (String.str ch) ^ "\" }"

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

fun firstChar "" = ("", Char.chr 26)
  | firstChar s =
      if String.size s > 1 then
        (String.extract (s, 1, NONE), String.sub (s, 0))
      else
        ("", String.sub(s, 0))

(* val read_char: Lexer -> Lexer * char *)
(* takes a lexer, reads extracts the first char from it, and returns the char
 * and an updated lexer *)
fun read_char l =
  let val { input=input, ch=ch } = l
      val (remainder, c) = firstChar input
      val newLexer       = { input=remainder, ch=c }
  in
    newLexer
  end

(* val read_identifier : Lexer -> Lexer * Token *)
fun read_identifier l =
  let fun ri (s, c) =
        if (Char.isAlpha c) orelse (c = #"_")
        then (String.str c) ^ (ri (firstChar s))
        else ""

      val { input=input, ch=ch } = l
      val identifier             = ri (input, ch)
      val remainder = String.extract (input, (String.size identifier) - 1, NONE)
      val c = String.sub(remainder, 0)
  in
    (read_char { input=remainder, ch=c  }, (IDENT identifier))
  end

(* val read_number : Lexer -> Lexer * Token *)
fun read_number l =
  let fun rn (s, c) =
        if (Char.isDigit c)
        then (String.str c) ^ (rn (firstChar s))
        else ""

      val { input=input, ch=ch } = l

      val number                 = rn (input, ch)
      val remainder = String.extract (input, (String.size number) - 1, NONE)

      val c = String.sub(remainder, 0) (* handle Subscript => #"\^Z"  *)
  in
    (read_char { input=remainder, ch=c  }, (INT number))
  end

(* val read_string : Lexer -> Lexer * Token *)
fun read_string l =
  let fun rs (s, c) =
        if (not (c = #"\""))
        then (String.str c) ^ (rs (firstChar s))
        else ""

      val { input=input, ch=ch } = l
      val (input, ch)            = firstChar input
      val str                    = rs (input, ch)
      val remainder = String.extract (input, (String.size str) - 1, NONE)
      val c = String.sub(remainder, 0)
  in
    (* 🤔 *) 
    (read_char (read_char { input=remainder, ch=c  }), (STRING str))
  end

(* val next_token: Lexer -> Lexer * Token *)
fun next_token l =
  let val l2 = read_char l
      val { ch=c, ... }: Lexer = l

      fun nt #"\^Z" = (l2, EOF)
        | nt #","   = (l2, COMMA)
        | nt #";"   = (l2, SEMICOLON)
        | nt #"("   = (l2, LPAREN)
        | nt #")"   = (l2, RPAREN)
        | nt #"{"   = (l2, LSQUIRLY)
        | nt #"}"   = (l2, RSQUIRLY)
        | nt #"["   = (l2, LBRACKET)
        | nt #"]"   = (l2, RBRACKET)
        | nt #"="   = (l2, ASSIGN)
        | nt #"+"   = (l2, PLUS)
        | nt #"-"   = (l2, MINUS)
        | nt #"/"   = (l2, SLASH)
        | nt #"*"   = (l2, ASTERISK)
        | nt #"<"   = (l2, LT)
        | nt #">"   = (l2, GT)
        | nt #"_"   = (read_identifier l)
        | nt #"\""  = (read_string l)
        (* if only SML supported guards... :( *)
        | nt ch     = if (Char.isAlpha ch)
                      then (read_identifier l)
                      else if (Char.isDigit ch)
                           then (read_number l)
                           else (l, ILLEGAL)

      fun clear_whitespace { input=input, ch=ch } =
        if (ch = #" ") then
          read_char { input=input, ch=ch }
        else
          { input=input, ch=ch }

      val (new_lexer, t)       = nt c
  in
    (clear_whitespace new_lexer, t)
  end

(* PARSING *)
type Parser = { lexer:      Lexer,
                curr_token: Token,
                peek_token: Token,
                errors:     string list }

fun parserString parser =
  let val { curr_token=curr_token,
            peek_token=peek_token,
            errors=errors, ... }: Parser = parser
  in
    "{ curr_token=" ^ (tokenString curr_token) ^ ", peek_token="
    ^ (tokenString peek_token) ^ " }"
  end

(* val new_parser : Lexer -> Parser *)
fun new_parser l =
  let val (l, firstToken)     = next_token l
      val (l, secondToken)    = next_token l
      val errors: string list = []
  in
    { curr_token=firstToken, peek_token=secondToken, errors=errors, lexer=l }
  end

fun p_next_token p =
  let val { lexer=lexer, peek_token=peek_token, errors=errors, ... }: Parser = p
      val (lexer, nt) = next_token lexer
  in
    { lexer=lexer, curr_token=peek_token, peek_token=nt, errors=errors } 
  end

(* TESTING *)
(*
(* {{{ *)
val lexer = { input = "et super_long_var_name = 100 + 50;", ch = #"l" }
val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)
val () = println ""

val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)
val () = println ""

val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)
val () = println ""

val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)
val () = println ""

val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)
val () = println ""

val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)
val () = println ""

val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)

val () = (println ""; println "--------------------------"; println "")

val lexer = { input = "some lengthy string\" = \"another string\"", ch = #"\"" }
val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)
val () = println ""

val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)
val () = println ""

val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)

val () = (println ""; println "--------------------------"; println "")

val lexer = { input = "et super_long_var_name \"a sneaky string\" = 100 + 50;", ch = #"l" }
val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)
val () = println ""

val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)
val () = println ""

val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)
val () = println ""

val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)
val () = println ""

val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)
val () = println ""

val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)
val () = println ""

val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)
val () = println ""

val () = println (lexerString lexer)
val (lexer, t) = next_token lexer
val () = println (tokenString t)
(* }}} *)
*)

val lexer = { input = "et super_long_var_name = 100 + 50;", ch = #"l" }
val parser = new_parser lexer
val () = println (parserString parser)
val parser = p_next_token parser
val () = println (parserString parser)
val parser = p_next_token parser
val () = println (parserString parser)
