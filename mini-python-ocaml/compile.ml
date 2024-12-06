
open Format
open X86_64
open Ast

let debug = ref false

module StringMap = Map.Make(String)

type env_t = {
  vars: (var * int) StringMap.t;
  funcs: fn StringMap.t;
  mutable stack_offset: int;
}

let empty_env = {
  vars = StringMap.empty;
  funcs = StringMap.empty;
  stack_offset = 0;
}

let compile_const (c: Ast.constant) : X86_64.text =
  match c with
  | Cint i ->
    movq (imm64 i) (reg rax)
  | Cbool b ->
    movq (imm64 (if b then 1L else 0L)) (reg rax)
  | Cstring s ->
    failwith "Unsupported constant"
  | _ ->
    failwith "Unsupported constant"

let compile_expr (env: env_t) (expr: Ast.texpr) : X86_64.text =
  match expr with
  | TEcst c ->
    compile_const c
  | _ ->
    failwith "Unsupported expression"

let rec compile_stmt (env: env_t) (stmt: Ast.tstmt) : X86_64.text =
  match stmt with
  | TSassign (var, expr) ->
    let offset:int =
      if StringMap.mem var.v_name env.vars then
        snd (StringMap.find var.v_name env.vars)
      else
        let new_offset = env.stack_offset - 8 in
        env.stack_offset <- new_offset;
        new_offset
    in
    let expr_code:X86_64.text = compile_expr env expr in
    expr_code ++ movq (reg rax) (ind ~ofs:offset rbp)
  | TSblock stmts ->
    List.fold_left (fun acc stmt -> acc ++ compile_stmt env stmt) nop stmts
  | TSprint expr ->
    let expr_code = compile_expr env expr in
    expr_code ++ movq (reg rax) (reg rdi) ++
    call "print_int"
  | _ ->
    failwith "Unsupported statement"

let compile_def env ((fn, body): Ast.tdef) : X86_64.text =
  let env = { env with vars = StringMap.empty; stack_offset = 0 } in
  let prologue = pushq (reg rbp) ++
                 movq (reg rsp) (reg rbp) in
  let epilogue = leave in
  let body_code = compile_stmt env body in
  begin match fn.fn_name with
    | _ ->
      label fn.fn_name ++ prologue ++ body_code ++ epilogue
  end

let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  let env = empty_env in
  (* Compile each function *)
  let text_code, data_code = List.fold_left (fun (text_acc, data_acc) (fn, body) ->
    let fn_code = compile_def env (fn, body) in
    (text_acc ++ fn_code, data_acc)
  ) (nop, nop) p in
  { text = globl "main" ++ text_code ++ ret;
    data = data_code;
  }