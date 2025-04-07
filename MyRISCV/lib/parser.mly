%{
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
%}

%token <int> INT
%token <string> VAR
%token <bool> BOOL
%token PLUS MINUS TIMES DIV LPAREN RPAREN 
%token EQUAL LET IN IF THEN ELSE FUNC ARROW UMINUS EOF APP

%nonassoc LET EQUAL IN 
%nonassoc IF THEN ELSE 
%nonassoc FUNC ARROW 
%nonassoc INT VAR BOOL LPAREN 
%left APP
%left PLUS MINUS
%left TIMES DIV
%right UMINUS


%start main
%type <Ast.expr> main

%%

main:
  expr EOF { $1 }

expr:
  | expr PLUS expr   { Binop(Add, $1, $3) }
  | expr MINUS expr  { Binop(Sub, $1, $3) }
  | expr TIMES expr  { Binop(Mul, $1, $3) }
  | expr DIV expr    { Binop(Div, $1, $3) }
  | LET VAR EQUAL expr IN expr { Let($2, $4, $6) }
  | IF expr THEN expr ELSE expr { If($2, $4, $6) }
  | FUNC VAR ARROW expr{ Func($2, $4) }
  | app_expr         %prec APP{ $1 }

app_expr:
  | app_expr atom    { App($1, $2) }
  | atom             { $1 }

atom:
  | INT              { Int($1) }
  | VAR              { Var($1) }
  | BOOL             { Bool($1) }
  | LPAREN expr RPAREN { $2 }
  | UMINUS atom      { Binop(Sub, Int(0), $2) }
  

%%
