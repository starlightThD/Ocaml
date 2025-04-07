open Lib

let rec string_of_expr (e : Ast.expr) : string = 
  match e with
  | Int n -> Printf.sprintf "Int %d" n
  | Var id -> Printf.sprintf "Var %s" id
  | Bool b -> 
    let b_str = 
      match b with 
      | true -> "true"
      | false -> "false"
    in
    Printf.sprintf "Bool %s" b_str
  | Binop (binop, e1, e2) ->
    let binop_str = 
      match binop with 
      | Add -> "Add"
      | Mul -> "Mul"
      | Sub -> "Sub"
      | Div -> "Div"
      | Leq -> "Leq"
    in
    Printf.sprintf "Binop (%s, %s, %s)" binop_str (string_of_expr e1) (string_of_expr e2)
  | Let (var, e1, e2) -> Printf.sprintf "Let (%s, %s, %s)" var (string_of_expr e1) (string_of_expr e2)
  | If (e1, e2, e3) -> Printf.sprintf "If (%s, %s, %s)" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | Func (var, body) -> Printf.sprintf "Func (%s, %s)" var (string_of_expr body)
  | App (e1, e2) -> Printf.sprintf "App (%s, %s)" (string_of_expr e1) (string_of_expr e2)

let parse s : Ast.expr =
  try 
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.read lexbuf in
  ast
with
| Failure msg -> 
  Printf.eprintf "compiler error:%s\n" msg;
  exit 1


(* 生成唯一标签的辅助函数 *)
let label_counter = ref 0
let new_label () = 
  let id = !label_counter in
  incr label_counter;
  ".L" ^ string_of_int id

(* 全局列表：保存所有生成的函数代码，最终附加在程序末尾 *)
let functions : string list ref = ref []

(* 简单的自由变量分析（不去重，仅适用于教学示例） *)
(* let rec free_vars expr:Ast.expr bound = 
  match expr with
  | Int _ | Bool _ -> []
  | Var x -> if List.mem x bound then [] else [x]
  | Binop (_, e1, e2) -> free_vars e1 bound @ free_vars e2 bound
  | Let (x, e1, e2) -> free_vars e1 bound @ free_vars e2 (x :: bound)
  | If (cond, e_then, e_else) ->
    free_vars cond bound @ free_vars e_then bound @ free_vars e_else bound
  | Func (x, body) -> free_vars body (x :: bound)
  | App (e1, e2) -> free_vars e1 bound @ free_vars e2 bound *)

(*
  compile_expr env cur_offset expr
  env: (variable, offset) 的关联列表，其中 offset 是相对于 fp 的偏移（单位：字节）
  cur_offset: 当前已经分配的 let 变量字节数（每个变量占 8 字节）
  返回的汇编代码保证计算结果存放在寄存器 a0 中
*)
(* 编译表达式 *)
    let rec compile_expr (env : (string * int) list) (cur_offset : int) (expr : Ast.expr) : string = 
      match expr with
      | Int n -> 
        Printf.sprintf "\tli a0, %d\n" n
      | Bool b ->
        if b then "\tli a0, 1\n" else "\tli a0, 0\n"
      | Var x ->
        (try
          let offset = List.assoc x env in
          Printf.sprintf "\tld a0, -%d(fp)\n" offset
        with Not_found ->
          failwith ("Unbound variable: " ^ x))
      
      (* Let 表达式：x = e1 in e2 *)
      | Let (x, e1, e2) ->
        let e1_code = compile_expr env cur_offset e1 in
        let store_code = Printf.sprintf "\tsd a0, -%d(fp)\n" cur_offset in
        let new_env = (x, cur_offset) :: env in
        let e2_code = compile_expr new_env (cur_offset + 8) e2 in  (* 关键：cur_offset + 8 *)
        e1_code ^ store_code ^ e2_code
      
      (* 二元操作：e1 op e2 *)
      | Binop (op, e1, e2) ->
        let e1_code = compile_expr env cur_offset e1 in
        let e2_code = compile_expr env cur_offset e2 in  (* 修复：使用相同 cur_offset *)
        let op_code = match op with
          | Add -> "\tadd a0, a0, a1\n"
          | Sub -> "\tsub a0, a0, a1\n"
          | Mul -> "\tmul a0, a0, a1\n"
          | Div -> "\tdiv a0, a0, a1\n"
          | Leq -> "\tseq a0, a0, a1\n"
        in
        e1_code ^
        "\taddi sp, sp, -8\n" ^    (* 保存 e1 结果到栈 *)
        "\tsd a0, 0(sp)\n" ^
        e2_code ^
        "\tmv a1, a0\n" ^          (* e2 结果存入 a1 *)
        "\tld a0, 0(sp)\n" ^       (* 恢复 e1 结果到 a0 *)
        "\taddi sp, sp, 8\n" ^
        op_code
      
      (* 条件表达式：if cond then e1 else e2 *)
      | If (cond, e_then, e_else) ->
        let cond_code = compile_expr env cur_offset cond in
        let else_label = new_label () in
        let end_label = new_label () in
        let then_code = compile_expr env cur_offset e_then in
        let else_code = compile_expr env cur_offset e_else in
        cond_code ^
        "\tbeqz a0, " ^ else_label ^ "\n" ^  (* 条件跳转 *)
        then_code ^
        "\tj " ^ end_label ^ "\n" ^
        else_label ^ ":\n" ^
        else_code ^
        end_label ^ ":\n"
        (* 函数定义：fun x -> body *)
  | Func (x, body) ->
    let func_label = new_label () in
    let param_offset = cur_offset + 8 in  (* 参数相对于 fp 的偏移 *)
    let body_code = compile_expr ((x, param_offset) :: env) (param_offset + 8) body in
    (* 函数入口 *)
    func_label ^ ":\n" ^
    "\taddi sp, sp, -16\n" ^
    "\tsd ra, 8(sp)\n" ^
    "\tsd fp, 0(sp)\n" ^
    "\tmv fp, sp\n" ^
    body_code ^
    "\tld ra, 8(sp)\n" ^
    "\tld fp, 0(sp)\n" ^
    "\taddi sp, sp, 16\n" ^
    "\tret\n"
      (* 函数应用：e1 e2 *)
      | App (e1, e2) ->
        let e1_code = compile_expr env cur_offset e1 in
        let e2_code = compile_expr env cur_offset e2 in  (* 修复：使用相同 cur_offset *)
        e1_code ^
        "\taddi sp, sp, -8\n" ^
        "\tsd a0, 0(sp)\n" ^      (* 保存函数地址 *)
        e2_code ^
        "\tmv a1, a0\n" ^         (* 参数存入 a1 *)
        "\tld a0, 0(sp)\n" ^      (* 恢复函数地址到 a0 *)
        "\taddi sp, sp, 8\n" ^
        "\tjalr a0\n"             (* 跳转并链接 *)
      
      (* 其他表达式处理... *)
    

(* and compile_expr_func (local_env : (string * int) list) (closure_env : (string * int) list) (cur_offset : int) (expr : Ast.expr) : string =
  match expr with
  | _ -> failwith "TODO" *)

let compiler_program (e : Ast.expr) : string =
  let body_code = compile_expr [] 0 e in
  let prologue = 
    ".text\n\
    .global main\n\
    main:\n\
    \taddi sp, sp, -64\n\
    \tmv fp, sp\n"
  in
  let epilogue = 
    "\
    \tmv sp, fp\n\
    \taddi sp, sp, 64\n\
    \tret\n"
  in
  let func_code = String.concat "\n" !functions in
  prologue ^ body_code ^ epilogue ^ "\n" ^ func_code
  
let () =
  let filename = "test/simpl_test.in" in
  (* let filename = "test/simpl_test2.in" in *)
  let in_channel = open_in filename in
  let file_content = really_input_string in_channel (in_channel_length in_channel) in
  close_in in_channel;

  (* let res = interp file_content in
  Printf.printf "Result of interpreting %s:\n%s\n\n" filename res;

  let res = interp_big file_content in
  Printf.printf "Result of interpreting %s with big-step model:\n%s\n\n" filename res; *)

  let ast = parse file_content in 
  Printf.printf "AST: %s\n" (string_of_expr ast);

  let output_file = Sys.argv.(1) in
  let oc = open_out output_file in

  let asm_code = compiler_program ast in

  output_string oc asm_code;
  close_out oc;
  Printf.printf "Generated RISC-V code saved to: %s\n" output_file