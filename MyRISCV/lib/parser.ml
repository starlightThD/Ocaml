type token =
  | INT of (
# 17 "./parser.mly"
        int
# 6 "./parser.ml"
)
  | VAR of (
# 18 "./parser.mly"
        string
# 11 "./parser.ml"
)
  | BOOL of (
# 19 "./parser.mly"
        bool
# 16 "./parser.ml"
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

open Parsing
let _ = parse_error;;
# 2 "./parser.mly"
    open Ast
    (* 自定义错误处理函数 *)
  let parse_error msg =
    let start_pos = Parsing.symbol_start_pos () in
    let end_pos = Parsing.symbol_end_pos () in
    let line = start_pos.Lexing.pos_lnum in
    let start_col = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
    let end_col = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol in
    let error_msg = 
      Printf.sprintf "syntax error:%s\n position:row %d ,col %d-%d" 
        msg line start_col end_col
    in
    failwith error_msg
# 52 "./parser.ml"
let yytransl_const = [|
  260 (* PLUS *);
  261 (* MINUS *);
  262 (* TIMES *);
  263 (* DIV *);
  264 (* LPAREN *);
  265 (* RPAREN *);
  266 (* EQUAL *);
  267 (* LET *);
  268 (* IN *);
  269 (* IF *);
  270 (* THEN *);
  271 (* ELSE *);
  272 (* FUNC *);
  273 (* ARROW *);
  274 (* UMINUS *);
    0 (* EOF *);
  275 (* APP *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* VAR *);
  259 (* BOOL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\003\000\003\000\004\000\004\000\004\000\004\000\004\000\
\000\000"

let yylen = "\002\000\
\002\000\003\000\003\000\003\000\003\000\006\000\006\000\004\000\
\001\000\002\000\001\000\001\000\001\000\001\000\003\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\012\000\013\000\014\000\000\000\000\000\000\000\
\000\000\000\000\017\000\000\000\000\000\011\000\000\000\000\000\
\000\000\000\000\016\000\000\000\000\000\000\000\000\000\001\000\
\010\000\015\000\000\000\000\000\000\000\000\000\000\000\004\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\011\000\012\000\013\000\014\000"

let yysindex = "\010\000\
\037\255\000\000\000\000\000\000\000\000\037\255\010\255\037\255\
\016\255\002\255\000\000\049\000\002\255\000\000\071\255\009\255\
\056\255\007\255\000\000\037\255\037\255\037\255\037\255\000\000\
\000\000\000\000\037\255\037\255\037\255\001\255\001\255\000\000\
\000\000\067\255\052\255\077\255\037\255\037\255\077\255\077\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\000\025\000\000\000\
\000\000\000\000\000\000\029\000\000\000\000\000\033\000\037\000"

let yygindex = "\000\000\
\000\000\250\255\000\000\252\255"

let yytablesize = 312
let yytable = "\015\000\
\009\000\017\000\003\000\004\000\005\000\019\000\022\000\023\000\
\025\000\006\000\001\000\016\000\002\000\030\000\031\000\032\000\
\033\000\018\000\027\000\010\000\034\000\035\000\036\000\029\000\
\003\000\000\000\000\000\000\000\008\000\000\000\039\000\040\000\
\006\000\000\000\000\000\000\000\007\000\003\000\004\000\005\000\
\000\000\000\000\000\000\000\000\006\000\000\000\000\000\007\000\
\024\000\008\000\000\000\000\000\009\000\000\000\010\000\020\000\
\021\000\022\000\023\000\020\000\021\000\022\000\023\000\000\000\
\000\000\000\000\038\000\000\000\000\000\028\000\020\000\021\000\
\022\000\023\000\020\000\021\000\022\000\023\000\037\000\026\000\
\020\000\021\000\022\000\023\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\009\000\009\000\009\000\009\000\
\000\000\009\000\000\000\000\000\009\000\000\000\009\000\009\000\
\002\000\002\000\000\000\000\000\000\000\002\000\000\000\000\000\
\002\000\000\000\002\000\002\000\003\000\003\000\000\000\000\000\
\000\000\003\000\000\000\000\000\003\000\008\000\003\000\003\000\
\008\000\006\000\008\000\008\000\006\000\007\000\006\000\006\000\
\007\000\000\000\007\000\007\000\020\000\021\000\022\000\023\000"

let yycheck = "\006\000\
\000\000\008\000\001\001\002\001\003\001\010\000\006\001\007\001\
\013\000\008\001\001\000\002\001\000\000\020\000\021\000\022\000\
\023\000\002\001\010\001\018\001\027\000\028\000\029\000\017\001\
\000\000\255\255\255\255\255\255\000\000\255\255\037\000\038\000\
\000\000\255\255\255\255\255\255\000\000\001\001\002\001\003\001\
\255\255\255\255\255\255\255\255\008\001\255\255\255\255\011\001\
\000\000\013\001\255\255\255\255\016\001\255\255\018\001\004\001\
\005\001\006\001\007\001\004\001\005\001\006\001\007\001\255\255\
\255\255\255\255\015\001\255\255\255\255\014\001\004\001\005\001\
\006\001\007\001\004\001\005\001\006\001\007\001\012\001\009\001\
\004\001\005\001\006\001\007\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\005\001\006\001\007\001\
\255\255\009\001\255\255\255\255\012\001\255\255\014\001\015\001\
\004\001\005\001\255\255\255\255\255\255\009\001\255\255\255\255\
\012\001\255\255\014\001\015\001\004\001\005\001\255\255\255\255\
\255\255\009\001\255\255\255\255\012\001\009\001\014\001\015\001\
\012\001\009\001\014\001\015\001\012\001\009\001\014\001\015\001\
\012\001\255\255\014\001\015\001\004\001\005\001\006\001\007\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  LPAREN\000\
  RPAREN\000\
  EQUAL\000\
  LET\000\
  IN\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FUNC\000\
  ARROW\000\
  UMINUS\000\
  EOF\000\
  APP\000\
  "

let yynames_block = "\
  INT\000\
  VAR\000\
  BOOL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 39 "./parser.mly"
           ( _1 )
# 232 "./parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 42 "./parser.mly"
                     ( Binop(Add, _1, _3) )
# 240 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 43 "./parser.mly"
                     ( Binop(Sub, _1, _3) )
# 248 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 44 "./parser.mly"
                     ( Binop(Mul, _1, _3) )
# 256 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 45 "./parser.mly"
                     ( Binop(Div, _1, _3) )
# 264 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 46 "./parser.mly"
                               ( Let(_2, _4, _6) )
# 273 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 47 "./parser.mly"
                                ( If(_2, _4, _6) )
# 282 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 48 "./parser.mly"
                       ( Func(_2, _4) )
# 290 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 49 "./parser.mly"
                              ( _1 )
# 297 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'app_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 52 "./parser.mly"
                     ( App(_1, _2) )
# 305 "./parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 53 "./parser.mly"
                     ( _1 )
# 312 "./parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 56 "./parser.mly"
                     ( Int(_1) )
# 319 "./parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "./parser.mly"
                     ( Var(_1) )
# 326 "./parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 58 "./parser.mly"
                     ( Bool(_1) )
# 333 "./parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 59 "./parser.mly"
                       ( _2 )
# 340 "./parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 60 "./parser.mly"
                     ( Binop(Sub, Int(0), _2) )
# 347 "./parser.ml"
               : 'atom))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.expr)
;;
