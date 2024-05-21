structure T = Token

signature LEXER = sig
  type Lexer

  val read_char: Lexer -> Lexer
  val read_identifier : Lexer -> Lexer * T.Token
  val read_number : Lexer -> Lexer * T.Token
  val read_string : Lexer -> Lexer * T.Token
  val next_token: Lexer -> Lexer * T.Token

  val lexerString : Lexer -> string
end

structure Lexer : LEXER =
struct
  type Lexer = {
    input: string,
    ch: char
  }

  fun lexerString { input=input, ch=ch } =
    "{ input=\"" ^ input ^ "\", ch=#\"" ^ (String.str ch) ^ "\" }"

  fun firstChar "" = ("", Char.chr 26)
    | firstChar s =
        if String.size s > 1 then
          (String.extract (s, 1, NONE), String.sub (s, 0))
        else
          ("", String.sub(s, 0))

  (* takes a lexer, reads extracts the first char from it, and returns the char
   * and an updated lexer *)
  fun read_char l =
    let val { input=input, ch=ch } = l
        val (remainder, c) = firstChar input
        val newLexer       = { input=remainder, ch=c }
    in
      newLexer
    end

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
      (read_char { input=remainder, ch=c  }, (T.IDENT identifier))
    end

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
      (read_char { input=remainder, ch=c  }, (T.INT number))
    end

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
      (read_char (read_char { input=remainder, ch=c  }), (T.STRING str))
    end

  fun next_token l =
    let val l2 = read_char l
        val { ch=c, ... }: Lexer = l

        fun clear_whitespace { input=input, ch=ch } =
          if (ch = #" ") then
            read_char { input=input, ch=ch }
          else
            { input=input, ch=ch }

        fun nt #"\^Z" = (l2, T.EOF)
          | nt #","   = (l2, T.COMMA)
          | nt #";"   = (l2, T.SEMICOLON)
          | nt #"("   = (l2, T.LPAREN)
          | nt #")"   = (l2, T.RPAREN)
          | nt #"{"   = (l2, T.LSQUIRLY)
          | nt #"}"   = (l2, T.RSQUIRLY)
          | nt #"["   = (l2, T.LBRACKET)
          | nt #"]"   = (l2, T.RBRACKET)
          | nt #"="   = (l2, T.ASSIGN)
          | nt #"+"   = (l2, T.PLUS)
          | nt #"-"   = (l2, T.MINUS)
          | nt #"/"   = (l2, T.SLASH)
          | nt #"*"   = (l2, T.ASTERISK)
          | nt #"<"   = (l2, T.LT)
          | nt #">"   = (l2, T.GT)
          | nt #" "   = next_token (clear_whitespace l2)
          | nt #"_"   = (read_identifier l)
          | nt #"\""  = (read_string l)
          (* if only SML supported guards... :( *)
          | nt ch     = if (Char.isAlpha ch)
                        then (read_identifier l)
                        else if (Char.isDigit ch)
                             then (read_number l)
                             else (l, T.ILLEGAL)

        val (new_lexer, t) = nt c
    in
      (new_lexer, t)
    end
end
