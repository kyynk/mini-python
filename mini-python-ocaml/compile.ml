
open Format
open X86_64
open Ast

let debug = ref false

module StringMap = Map.Make(String)

type env_t = {
  mutable vars: (var * int) StringMap.t;
  funcs: fn StringMap.t;
  mutable stack_offset: int;
  mutable string_counter: int;
}

let empty_env = {
  vars = StringMap.empty;
  funcs = StringMap.empty;
  stack_offset = 0;
  string_counter = 0;
}

let tag_none = Int64.of_int 0
let tag_bool = Int64.of_int 1
let tag_int = Int64.of_int 2
let tag_str = Int64.of_int 3

let unique_string_label env =
  let lbl = Printf.sprintf ".Lstr%d" env.string_counter in
  env.string_counter <- env.string_counter + 1;
  lbl

let compile_const (c: Ast.constant) : X86_64.text =
  match c with
  | Cint i ->
    movq (imm64 i) (ind ~ofs:(0) rax)
  | _ ->
    failwith "Unsupported constant"

let compile_expr (env: env_t) (expr: Ast.texpr) : X86_64.text =
  match expr with
  | TEcst c ->
    compile_const c
  | TEvar v ->
    let var, ofs = StringMap.find v.v_name env.vars in
    movq (ind ~ofs:(-ofs) rbp) (reg rax)
  | _ ->
    failwith "Unsupported expression"

let rec compile_stmt (env: env_t) (stmt: Ast.tstmt) : X86_64.text =
  match stmt with
  | TSassign (var, expr) ->
    if StringMap.mem var.v_name env.vars then
      let var, ofs = StringMap.find var.v_name env.vars in
      let expr_code = compile_expr env expr in
      expr_code ++ movq (reg rax) (ind ~ofs:(-ofs) rbp)
    else
      (* create a new var in env *)
      let ofs = env.stack_offset - 8 in
      env.stack_offset <- ofs;
      env.vars <- StringMap.add var.v_name (var, ofs) env.vars;
      (* compile *)
      let expr_code = compile_expr env expr in
      movq (imm 8) (reg rdi) ++
      call "my_malloc" ++
      subq (imm 8) (reg rbp) ++
      movq (reg rax) (ind ~ofs:(ofs) rbp) ++
      expr_code
  | TSblock stmts ->
    List.fold_left (fun acc stmt -> acc ++ compile_stmt env stmt) nop stmts
  | TSprint expr ->
    let expr_code = compile_expr env expr in
    expr_code ++
    movq (ind ~ofs:(-8) rbp) (reg rax) ++
    movq (ind rax) (reg rsi) ++
    leaq (lab "print_int") rdi ++
    call "printf" ++
    addq (imm 8) (reg rbp)
  | _ ->
    failwith "Unsupported statement"
let compile_def env ((fn, body): Ast.tdef) : X86_64.text =
  let env = { env with vars = StringMap.empty; stack_offset = 0 } in
  let prologue = 
    pushq (reg rbp) ++
    movq (reg rsp) (reg rbp) in
  let epilogue = 
    testq (reg rax) (reg rax) ++
    movq (reg rbp) (reg rsp) ++
    popq rbp ++
    ret in
  let body_code = compile_stmt env body in
  label fn.fn_name ++ prologue ++ body_code ++ epilogue

let custom_malloc : X86_64.text =
  label "my_malloc" ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  andq (imm (-16)) (reg rsp) ++
  call "malloc" ++
  testq (reg rax) (reg rax) ++
  movq (reg rbp) (reg rsp) ++
  popq rbp ++
  ret

let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  let env = empty_env in
  (* Compile each function *)
  let text_code, data_code = List.fold_left (fun (text_acc, data_acc) (fn, body) ->
    let fn_code = compile_def env (fn, body) in
    (text_acc ++ fn_code, data_acc)
  ) (nop, nop) p in
  { text = 
  custom_malloc ++
  globl "main" ++ text_code;
    data = data_code ++
    label "print_int" ++
    string " %d\n"
  }