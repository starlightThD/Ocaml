type token =
  | INT of (
# 6 "./parser.mly"
        int
# 6 "./parser.mli"
)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LPAREN
  | RPAREN
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
