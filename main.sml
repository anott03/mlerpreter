fun println s = (print s; print "\n")
fun eprint   s = TextIO.output (TextIO.stdErr, s)
fun eprintln s = (eprint s; eprint "\n")

(* STRING UTILS *)
fun intString n =
  String.map (fn #"~" => #"-" | c => c) (Int.toString n)

fun boolString b = if b then "true" else "false"

structure MyLexer = struct
  structure L = Lexer
end
structure P = ParserNew(MyLexer)
structure A = Ast

val lexer  = { input = "let super_long_var_name = 100 + 50;", ch = #" " }
val parser = P.new_parser lexer

val statements: A.Node list = []
val prog: A.Program = { statements=statements }
val prog = P.parse_program (parser, prog)

val () = println("PROGRAM LENGTH: " ^ intString(length (#statements prog)))
