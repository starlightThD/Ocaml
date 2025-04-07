type expr = 
| Num of int
| Add of expr * expr
| Sub of expr * expr
| Mul of expr * expr
| Div of expr * expr
| Neg of expr

let rec show_expr = function
  | Num n -> string_of_int n
  | Add (e1, e2) -> Printf.sprintf "(%s + %s)" (show_expr e1) (show_expr e2)
  | Sub (e1, e2) -> Printf.sprintf "(%s - %s)" (show_expr e1) (show_expr e2)
  | Mul (e1, e2) -> Printf.sprintf "(%s * %s)" (show_expr e1) (show_expr e2)
  | Div (e1, e2) -> Printf.sprintf "(%s / %s)" (show_expr e1) (show_expr e2)
  | Neg e -> Printf.sprintf "-%s" (show_expr e)
