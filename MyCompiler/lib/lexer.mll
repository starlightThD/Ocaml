{
   (*header*)
   open Parser (*use Parser.token*)
   exception Unknown_character
}
(*rules*)
rule entrypoint = parse
(*| regexp {action}*)
| [' ' '\t' '\n'] { entrypoint lexbuf}
| ['0'-'9'] + as num {INT (int_of_string num)}
| '+' {PLUS}
| '-' {MINUS}
| '*' {TIMES}
| '/' {DIV}
| '(' {LPAREN}
| ')' {RPAREN}
| eof {EOF}
| _ {raise Unknown_character}

