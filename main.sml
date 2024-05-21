fun println s = (print s; print "\n")
fun eprint   s = TextIO.output (TextIO.stdErr, s)
fun eprintln s = (eprint s; eprint "\n")

(* STRING UTILS *)
fun intString n =
  String.map (fn #"~" => #"-" | c => c) (Int.toString n)

fun boolString b =
  String.concat [case b of true => "true" | false => "false"];

structure MyLexer = struct
  structure L = Lexer
end
structure P = ParserNew(MyLexer)

val lexer  = { input = "let super_long_var_name = 100 + 50;", ch = #" " }
val parser = P.new_parser lexer

val () = println (P.parserString parser)
val parser = P.next_token parser
val () = println (P.parserString parser)
val parser = P.next_token parser
val () = println (P.parserString parser)
