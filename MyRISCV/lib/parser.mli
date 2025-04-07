type token =
  | INT of (
# 17 "./parser.mly"
        int
# 6 "./parser.mli"
)
  | VAR of (
# 18 "./parser.mly"
        string
# 11 "./parser.mli"
)
  | BOOL of (
# 19 "./parser.mly"
        bool
# 16 "./parser.mli"
)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LPAREN
  | RPAREN
  | EQUAL
  | LET
  | IN
  | IF
  | THEN
  | ELSE
  | FUNC
  | ARROW
  | UMINUS
  | EOF
  | APP

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
