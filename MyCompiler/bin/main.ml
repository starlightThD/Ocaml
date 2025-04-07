open Lib

let parse_and_print input =
    let lexbuf = Lexing.from_string input in
    let ast = Parser.main Lexer.entrypoint lexbuf in
    Printf.printf "Parsed AST:\n%s\n" (Ast.show_expr ast) 

(* 主函数：读取输入并测试 *)
let () =
  let test_input = "3 + 4 * - 5 + 2" in
  parse_and_print test_input