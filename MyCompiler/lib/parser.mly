%{
    (*header*)
    open Ast
%}
(*declarations*)
%token <int> INT 
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOF
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS
%start main
%type <Ast.expr> main
%%
(*rules*)
main:
(*|symbol … symbol { semantic-action }*)
expr EOF {$1}
;
expr:
| term PLUS expr {Add($1,$3)}
| term MINUS expr {Sub($1,$3)}
| term { $1 }
;
term:
| factor TIMES term {Mul($1,$3)}
| factor DIV term {Div($1,$3)}
| factor {$1}
;
factor:
| INT {Num($1)}
| MINUS factor %prec UMINUS {Neg($2)}
| LPAREN expr RPAREN {$2}
;
%%
(*trailer*)