
open Format
open X86_64
open Ast

let debug = ref false

module StringMap = Map.Make(String)

type ty = [ `none | `int | `bool | `string | `list ]

type env_t = {
  mutable vars: (var * int * ty) StringMap.t;
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
let byte = 8

let unique_string_label (env:env_t) (s:label) : X86_64.data * label =
  let n = env.string_counter in
  env.string_counter <- n + 1;
  label (Printf.sprintf "str%d" n) ++ string s
  , "str" ^ (string_of_int n)

let compile_const (env: env_t) (c: Ast.constant) : X86_64.text * X86_64.data * ty =
  match c with
  | Cnone ->
    movq (imm (2 * byte)) (reg rdi) ++
    call "malloc_wrapper" ++
    movq (imm 0) (ind rax) ++
    movq (imm 0) (ind ~ofs:(byte) rax)
    , nop, `none
  | Cbool b ->
    movq (imm (2 * byte)) (reg rdi) ++
    call "malloc_wrapper" ++
    movq (imm 1) (ind rax) ++
    movq (imm (if b then 1 else 0)) (ind ~ofs:(byte) rax)
    , nop, `bool
  | Cint i ->
    movq (imm (2 * byte)) (reg rdi) ++
    call "malloc_wrapper" ++
    movq (imm 2) (ind rax) ++
    movq (imm64 i) (ind ~ofs:(byte) rax)
    , nop, `int
  | Cstring s ->
    (* let data_code, label = unique_string_label env s in *)
    let len = String.length s in
    let (text_code, _) = String.fold_left (fun (acc, counter) i ->
      let counter = counter + 8 in
      acc ++ movq (imm (Char.code i)) (ind ~ofs:(counter) rax), counter
    ) (nop, 2 * byte) s in
    movq (imm ((len + 3) * byte)) (reg rdi) ++
    call "malloc_wrapper" ++
    movq (imm 3) (ind rax) ++
    movq (imm len) (ind ~ofs:(byte) rax) ++
    text_code
    ,nop , `string

let compile_var (env: env_t) (v: Ast.var) : X86_64.text * X86_64.data * ty =
    let var, ofs, var_type = StringMap.find v.v_name env.vars in
      movq (ind ~ofs:(ofs) rbp) (reg rax), nop, var_type

let arith_asm (code1:X86_64.text) (code2:X86_64.text) (instruction:X86_64.text) : X86_64.text =
  code1 ++
  movq (ind rax) (reg rdi) ++
  pushq (reg rdi) ++
  code2 ++
  popq rdi ++
  movq (ind rax) (reg rsi) ++
  instruction ++
  pushq (reg rdi) ++
  movq (imm byte) (reg rdi) ++
  call "malloc_wrapper" ++
  popq rdi ++
  movq (reg rdi) (ind rax)

let construct_texpr_list (len:int) : texpr list =
  let rec aux acc i =
    if i = len then acc
    else aux (TEcst (Cint 0L) :: acc) (i + 1)
  in
  aux [] 0

(* return value *)
let rec compile_expr (env: env_t) (expr: Ast.texpr) : X86_64.text * X86_64.data * ty =
  match expr with
  | TEcst c ->
    compile_const env c
  | TEvar v ->
    compile_var env v
  | TEbinop (op, e1, e2) ->
    begin match op with
    | Band | Bor ->
      (* lazy evaluation *)
      failwith "tbd"
    | Badd | Bsub | Bmul | Bdiv | Bmod | Beq | Bneq | Blt | Ble | Bgt | Bge ->
      let text_code1, data_code1, expr_type1 = compile_expr env e1 in
      let text_code2, data_code2, expr_type2 = compile_expr env e2 in
      begin match op, expr_type1, expr_type2 with
      | Badd, `int, `int ->
        arith_asm text_code1 text_code2 (addq (reg rsi) (reg rdi)),
        data_code1 ++ data_code2, `int
      | Bsub, `int, `int ->
        arith_asm text_code1 text_code2 (subq (reg rsi) (reg rdi)),
        data_code1 ++ data_code2, `int
      | Bmul, `int, `int ->
        arith_asm text_code1 text_code2 (imulq (reg rsi) (reg rdi)),
        data_code1 ++ data_code2, `int
      | Bdiv, `int, `int ->
        arith_asm text_code1 text_code2
        (
          movq (reg rdi) (reg rax) ++
          cqto ++
          movq (reg rsi) (reg rbx) ++
          idivq (reg rbx) ++
          movq (reg rax) (reg rdi)
        ),
        data_code1 ++ data_code2, `int
      | Bmod, `int, `int ->
        arith_asm text_code1 text_code2
        (
          movq (reg rdi) (reg rax) ++
          cqto ++
          movq (reg rsi) (reg rbx) ++
          idivq (reg rbx) ++
          movq (reg rdx) (reg rdi)
        ),
        data_code1 ++ data_code2, `int
      | Beq, `int, `int ->
        arith_asm text_code1 text_code2
        (
          cmpq (reg rsi) (reg rdi) ++
          sete (reg dil) ++
          movzbq (reg dil) rdi
        ),
        data_code1 ++ data_code2, `bool
      | Bneq, `int, `int ->
        arith_asm text_code1 text_code2
        (
          cmpq (reg rsi) (reg rdi) ++
          setne (reg dil) ++
          movzbq (reg dil) rdi
        ),
        data_code1 ++ data_code2, `bool
      | Blt, `int, `int ->
        arith_asm text_code1 text_code2
        (
          cmpq (reg rsi) (reg rdi) ++
          setl (reg dil) ++
          movzbq (reg dil) rdi
        ),
        data_code1 ++ data_code2, `bool
      | Ble, `int, `int ->
        arith_asm text_code1 text_code2
        (
          cmpq (reg rsi) (reg rdi) ++
          setle (reg dil) ++
          movzbq (reg dil) rdi
        ),
        data_code1 ++ data_code2, `bool
      | Bgt, `int, `int ->
        arith_asm text_code1 text_code2
        (
          cmpq (reg rsi) (reg rdi) ++
          setg (reg dil) ++
          movzbq (reg dil) rdi
        ),
        data_code1 ++ data_code2, `bool
      | Bge, `int, `int ->
        arith_asm text_code1 text_code2
        (
          cmpq (reg rsi) (reg rdi) ++
          setge (reg dil) ++
          movzbq (reg dil) rdi
        ),
        data_code1 ++ data_code2, `bool
      | _ ->
        failwith "Unsupported binop"
      end
    end
  | TEunop (op, e) ->
    begin match op with
      | Uneg ->
        let text_code, data_code, expr_type = compile_expr env e in
        begin match expr_type with
        | `int ->
          text_code ++
          movq (ind rax) (reg rdi) ++
          negq (reg rdi) ++
          movq (imm byte) (reg rsi) ++
          call "malloc_wrapper",
          data_code, `int
        | _ ->
          failwith "Unsupported Uneg"
        end
      | Unot ->
        let text_code, data_code, expr_type = compile_expr env e in
        begin match expr_type with
        | `bool ->
          text_code ++
          movq (ind rax) (reg rdi) ++
          xorq (imm 1) (reg rdi) ++
          movq (imm byte) (reg rsi) ++
          call "malloc_wrapper",
          data_code, `bool
        | _ ->
          failwith "Unsupported Unot"
        end
    end
  | TEcall (fn, args) ->
    failwith "Unsupported TEcall"
  | TElist l ->
    let len = List.length l in
    List.fold_left (fun (acc, counter) i ->
      let text_code, _, _ = compile_expr env i in
      acc ++
      pushq (reg rax) ++
      text_code ++
      movq (reg rax) (reg rsi) ++
      popq rax ++
      movq (reg rsi) (ind ~ofs:(2 * byte + 8 * counter) rax),
      counter + 1
    ) (nop, 0) l |> fun (text_code, _) ->
    movq (imm ((len + 3) * byte)) (reg rdi) ++
    call "malloc_wrapper" ++
    movq (imm 4) (ind rax) ++
    movq (imm len) (ind ~ofs:(byte) rax) ++
    text_code, nop, `list
  | TErange e ->
    failwith "Unsupported TErange"
  | TEget (e1, e2) ->
    failwith "Unsupported TEget"

let rec compile_stmt (env: env_t) (stmt: Ast.tstmt) : X86_64.text * X86_64.data =
  match stmt with
  | TSif (cond, s1, s2) ->
    let text_code_cond, data_code_cond, _ = compile_expr env cond in
    let text_code_s1, data_code_s1 = compile_stmt env s1 in
    let text_code_s2, data_code_s2 = compile_stmt env s2 in
    text_code_cond ++
    cmpq (imm 0) (ind rax) ++
    je "else" ++
    text_code_s2 ++
    jmp "end" ++
    label "else" ++
    text_code_s1 ++
    label "end"
    , data_code_cond ++ data_code_s1 ++ data_code_s2
  | TSreturn expr ->
    failwith "Unsupported Sreturn"
  | TSassign (var, expr) ->
    (* tbm *)
    env.stack_offset <- env.stack_offset - 8;
    let text_code, data_code, expr_type = compile_expr env expr in
    env.vars <- StringMap.add var.v_name (var, env.stack_offset, expr_type) env.vars;
    text_code ++
    movq (reg rax) (ind ~ofs:(env.stack_offset) rbp)
    , data_code
  | TSprint expr ->
    let text_code, data_code, expr_type = compile_expr env expr in
    begin match expr_type with
    | `int ->
      comment "print_int" ++
      text_code ++
      movq (ind rax) (reg rsi) ++
      leaq (lab "print_int") rdi ++
      call "printf_wrapper",
      data_code
    | `string ->
      comment "print_str" ++
      text_code ++
      movq (reg rax) (reg rsi) ++
      leaq (lab "print_str") rdi ++
      call "printf_wrapper",
      data_code
    | `bool ->
      comment "print_int" ++
      text_code ++
      movq (ind rax) (reg rsi) ++
      leaq (lab "print_int") rdi ++
      call "printf_wrapper",
      data_code
    | _ ->
      failwith "Unsupported print"
    end
  | TSblock stmts ->
    List.fold_left (fun (acc_text, acc_data) stmt ->
      let text_code, data_code = compile_stmt env stmt in
      acc_text ++ text_code, acc_data ++ data_code
    ) (nop, nop) stmts
    |> fun (text_code, data_code) -> 
      addq (imm (env.stack_offset)) (reg rsp) ++
      text_code ++
      subq (imm (env.stack_offset)) (reg rsp)
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
    xorq (reg rax) (reg rax) ++
    movq (reg rbp) (reg rsp) ++
    popq rbp ++
    ret in
  let body_code, data_code = compile_stmt env_local body in
  label fn.fn_name ++ prologue ++ body_code ++ epilogue, data_code

let c_standard_function_wrapper (l:string) (fn_name:string): X86_64.text =
  label l ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  andq (imm (-16)) (reg rsp) ++
  xorq (reg rax) (reg rax) ++
  call fn_name ++
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
      c_standard_function_wrapper "malloc_wrapper" "malloc" ++
      c_standard_function_wrapper "putchar_wrapper" "putchar" ++
      c_standard_function_wrapper "printf_wrapper" "printf" ++
      c_standard_function_wrapper "strcmp_wrapper" "strcmp" ++
      c_standard_function_wrapper "strcpy_wrapper" "strcpy" ++
      c_standard_function_wrapper "strcat_wrapper" "strcat" ++
      globl "main" ++ text_code;
    data = 
      data_code ++
      label "print_int" ++
      string "%d\n" ++
      label "print_str" ++
      string "%s\n"
      (* hard-coded print_int *)
  }
