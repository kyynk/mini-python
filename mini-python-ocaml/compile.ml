
open Format
open X86_64
open Ast

let debug = ref false

module StringMap = Map.Make(String)

type env_t = {
  mutable vars: (var * int * int) StringMap.t;
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

let unique_string_label (env:env_t) (s:string) : X86_64.data =
  let n = env.string_counter in
  env.string_counter <- n + 1;
  label (Printf.sprintf "str%d" n)

let compile_const (env: env_t) (c: Ast.constant) : X86_64.text * X86_64.data * int =
  match c with
  | Cnone ->
    (* tbd *)
    movq (imm 0) (ind ~ofs:(0) rax), nop, 0
  | Cbool b ->
    (* tbd *)
    movq (imm64 (if b then 1L else 0L)) (ind ~ofs:(0) rax), nop, 1
  | Cint i ->
    env.stack_offset <- env.stack_offset + 8;
    movq (imm 8) (reg rdi) ++
    call "my_malloc" ++
    movq (imm64 i) (ind ~ofs:(0) rax) ++
    movq (reg rax) (ind ~ofs:(-env.stack_offset) rbp)
    , nop, 2
  (* | Cstring s ->
    let text_code =
      leaq (lab (Printf.sprintf "str%d" (env.string_counter - 1))) rax in
    let data_code = 
      unique_string_label env s ++
      string s in
    text_code, data_code *)
  | _ -> failwith "Unsupported constant"

let compile_var (env: env_t) (v: Ast.var) : X86_64.text * X86_64.data * int =
  if StringMap.mem v.v_name env.vars then
    begin
      let var, ofs, var_type = StringMap.find v.v_name env.vars in
      (* if var_type = 1 then
        comment "bool" ++
        movq (ind ~ofs:(ofs) rbp) (reg rax), nop *)
      if var_type = 2 then
        comment "int" ++
        movq (ind ~ofs:(ofs) rbp) (reg rax), nop, var_type
      else
        failwith "Unsupported var type"
    end
  else
    failwith "Variable not found"
    (* let ofs = -env.stack_offset in
    env.vars <- StringMap.add v.v_name (v, ofs, 2) env.vars;
    movq (ind ~ofs:(ofs) rbp) (reg rax), nop, 2 *)

let compile_expr (env: env_t) (expr: Ast.texpr) : X86_64.text * X86_64.data * int =
  match expr with
  | TEcst c ->
    compile_const env c
  | TEvar v ->
    compile_var env v
  | TEbinop (op, e1, e2) ->
    failwith "Unsupported TEbinop"
  | TEunop (op, e) ->
    failwith "Unsupported TEunop"
  | TEcall (fn, args) ->
    failwith "Unsupported TEcall"
  | TElist l ->
    failwith "Unsupported TElist"
  | TErange e ->
    failwith "Unsupported TErange"
  | TEget (e1, e2) ->
    failwith "Unsupported TEget"

let rec compile_stmt (env: env_t) (stmt: Ast.tstmt) : X86_64.text * X86_64.data =
  match stmt with
  | TSif (cond, s1, s2) ->
    failwith "Unsupported Sif"
  | TSreturn expr ->
    failwith "Unsupported Sreturn"
  | TSassign (var, expr) -> (* x = 1 *)
    let text_code, data_code, expr_type = compile_expr env expr in
    let ofs = -env.stack_offset in
    env.vars <- StringMap.add var.v_name (var, ofs, expr_type) env.vars;
    text_code, data_code
  | TSprint expr ->
    let text_code, data_code, expr_type = compile_expr env expr in
    (* format string required *)
    text_code ++
    movq (ind rax) (reg rsi) ++
    leaq (lab "print_int") rdi ++
    call "printf", data_code
  | TSblock stmts ->
    List.fold_left (fun (acc_text, acc_data) stmt ->
      let text_code, data_code = compile_stmt env stmt in
      acc_text ++ text_code, acc_data ++ data_code
    ) (nop, nop) stmts
    |> fun (text_code, data_code) -> 
      subq (imm (env.stack_offset)) (reg rsp) ++
      text_code ++
      addq (imm (env.stack_offset)) (reg rsp)
      , data_code
  | TSfor (var, expr, body) ->
    failwith "Unsupported Sfor"
  | TSeval expr ->
    failwith "Unsupported TSeval"
  | TSset (e1, e2, e3) ->
    failwith "Unsupported TSset"

let compile_def env ((fn, body): Ast.tdef) : X86_64.text * X86_64.data =
  let env_local = { env with vars = StringMap.empty; } in
  let prologue = 
    pushq (reg rbp) ++
    movq (reg rsp) (reg rbp) in
  let epilogue = 
    testq (reg rax) (reg rax) ++
    movq (reg rbp) (reg rsp) ++
    popq rbp ++
    ret in
  let body_code, data_code = compile_stmt env_local body in
  label fn.fn_name ++ prologue ++ body_code ++ epilogue, data_code

let custom_malloc : X86_64.text =
  label "my_malloc" ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  andq (imm (-16)) (reg rsp) ++
  comment "allign rsp to 16 bytes" ++
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
    let fn_code, data_code = compile_def env (fn, body) in
    (text_acc ++ fn_code, data_acc ++ data_code)
  ) (nop, nop) p in
  { 
    text = 
      custom_malloc ++
      globl "main" ++ text_code;
    data = 
      data_code ++
      label "print_int" ++
      string " %d\n"
      (* hard-coded print_int *)
  }