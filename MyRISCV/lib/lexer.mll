{
   (*header*)
   open Parser (*use Parser.token*)
   (* 
   %token <int> INT
   %token <string> VAR
   %token <bool> BOOL
   %token PLUS MINUS TIMES DIV LPAREN RPAREN
   %token EQUALS LET IN IF THEN ELSE FUNC UMINUS EOF APP 
   *)

   (* 定义位置类型 *)
  type position = {
    line: int;
    column: int;
  }

  (* 辅助函数：更新位置 *)
  let update_pos lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { 
      pos with 
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum 
    }

  (* 获取当前位置 *)
  let get_pos lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    { line = pos.Lexing.pos_lnum; column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol }
}
(*rules*)
rule read  = parse
(*| regexp {action}*)
| [' ' '\t'] { read lexbuf}
| '\n'       { update_pos lexbuf; read lexbuf }  (* 换行时更新位置 *)
| ['0'-'9'] + as num {INT (int_of_string num)}
| "true" {BOOL true}
| "false" {BOOL false}
| '+' {PLUS}
| '-' {MINUS}
| '*' {TIMES}
| '/' {DIV}
| '(' {LPAREN}
| ')' {RPAREN}
| '=' {EQUAL}
| "->" {ARROW}
| "let" {LET}
| "in" {IN}
| "if" {IF}
| "then" {THEN}
| "else" {ELSE}
| "fun" {FUNC}
| "app" {APP}
| ['a'-'z'] + as var {VAR var}
| eof {EOF}
| _ as c          { 
      (* 捕获非法字符并抛出带位置的错误 *)
      let pos = Lexing.lexeme_start_p lexbuf in
      let msg = Printf.sprintf "invalid character '%c' (row %d, col %d)" 
          c (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol) in
      failwith msg 
    }

