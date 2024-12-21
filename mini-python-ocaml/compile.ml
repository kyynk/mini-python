
open Format
open X86_64
open Ast
open Utils
open Environment
open Functions

let debug = ref false

let compile_const (env: env_t) (c: Ast.constant) : X86_64.text * X86_64.data * ty =
  match c with
  | Cnone ->
    movq (imm (2 * byte))  !%rdi ++
    call "malloc_wrapper" ++
    movq (imm 0) (ind rax) ++
    movq (imm 0) (ind ~ofs:(byte) rax)
    , nop, `none
  | Cbool b ->
    movq (imm (2 * byte)) !%rdi ++
    call "malloc_wrapper" ++
    movq (imm 1) (ind rax) ++
    movq (imm (if b then 1 else 0)) (ind ~ofs:(byte) rax)
    , nop, `bool
  | Cint i ->
    movq (imm (2 * byte)) !%rdi ++
    call "malloc_wrapper" ++
    movq (imm 2) (ind rax) ++
    movq (imm64 i) (ind ~ofs:(byte) rax)
    , nop, `int
  | Cstring s ->
    let len = String.length s in
    let string_label = unique_label env "string" in
    movq (imm (3 * byte)) !%rdi ++
    call "malloc_wrapper" ++
    movq (imm 3) (ind rax) ++
    movq (imm len) (ind ~ofs:(byte) rax) ++
    leaq (lab string_label) rdi ++
    movq !%rdi (ind ~ofs:(2 * byte) rax)
    , label string_label ++ string s, `string len


let compile_var (env: env_t) (v: Ast.var) : X86_64.text * X86_64.data * ty =
  let var, ofs, var_type = StringMap.find v.v_name env.vars in
    movq (ind ~ofs:(ofs) rbp) !%rax, nop, var_type

(* return value *)
let rec compile_expr (env: env_t) (expr: Ast.texpr) : X86_64.text * X86_64.data * ty =
  match expr with
  | TEcst c ->
    compile_const env c
  | TEvar v ->
    compile_var env v
  | TEbinop (op, e1, e2) ->
    begin match op with
    | Band ->
      let text_code1, data_code1, _ = compile_expr env e1 in
      let text_code2, data_code2, _ = compile_expr env e2 in
      let l = unique_label env "cond_false" in
      text_code1 ++
      movq (ind ~ofs:(byte) rax) !%rdi ++
      cmpq (imm 0) !%rdi ++
      je l ++
      text_code2 ++
      label l, 
      data_code1 ++ data_code2, `bool  
    | Bor ->
      let text_code1, data_code1, expr_type1 = compile_expr env e1 in
      let text_code2, data_code2, expr_type2 = compile_expr env e2 in
      let l = unique_label env "cond_true" in
      text_code1 ++
      movq (ind ~ofs:(byte) rax) !%rdi ++
      cmpq (imm 0) !%rdi ++
      jne l ++
      text_code2 ++
      label l,
      data_code1 ++ data_code2, `bool
    | Badd | Bsub | Bmul | Bdiv | Bmod | Beq | Bneq | Blt | Ble | Bgt | Bge ->
      let text_code1, data_code1, expr_type1 = compile_expr env e1 in
      let text_code2, data_code2, expr_type2 = compile_expr env e2 in
      begin match op, expr_type1, expr_type2 with
      | Badd, `int, `int ->
        arith_asm text_code1 text_code2 (addq !%rsi !%rdi),
        data_code1 ++ data_code2, `int
      | Badd, `string l1, `string l2 ->
        let new_label = unique_label env "string" in
        text_code1 ++
        movq (ind ~ofs:(byte) rax) !%rdx ++
        movq (ind ~ofs:(2 * byte) rax) !%rsi ++
        leaq (lab new_label) rdi ++
        pushq !%rdx ++
        call "strcpy_wrapper" ++
        text_code2 ++
        popq rdx ++
        addq (ind ~ofs:(byte) rax) !%rdx ++
        movq (ind ~ofs:(2 * byte) rax) !%rsi ++
        pushq !%rdx ++
        call "strcat_wrapper" ++
        movq !%rax !%rdi ++
        pushq !%rdi ++
        movq (imm (3 * byte)) !%rdi ++
        call "malloc_wrapper" ++
        popq rdi ++
        popq rdx ++
        movq (imm 3) (ind rax) ++
        movq !%rdx (ind ~ofs:(byte) rax) ++
        movq !%rdi (ind ~ofs:(2 * byte) rax),
        data_code1 ++ data_code2 ++
        label new_label ++ space (l1+l2+1), `string (l1 + l2)
      | Badd, `list l1, `list l2 ->
        text_code1 ++
        movq !%rax !%rdi ++
        pushq !%rdi ++
        text_code2 ++
        movq !%rax !%rsi ++
        popq rdi ++
        call "concat_list", data_code1 ++ data_code2, `list (l1 + l2)
      | Bsub, `int, `int ->
        arith_asm text_code1 text_code2 (subq !%rsi !%rdi),
        data_code1 ++ data_code2, `int
      | Bmul, `int, `int ->
        arith_asm text_code1 text_code2 (imulq !%rsi !%rdi),
        data_code1 ++ data_code2, `int
      | Bdiv, `int, `int ->
        arith_asm text_code1 text_code2
        (
          movq !%rdi !%rax ++
          cqto ++
          movq !%rsi !%rbx ++
          idivq !%rbx ++
          movq !%rax !%rdi
        ),
        data_code1 ++ data_code2, `int
      | Bmod, `int, `int ->
        arith_asm text_code1 text_code2
        (
          movq !%rdi !%rax ++
          cqto ++
          movq !%rsi !%rbx ++
          idivq !%rbx ++
          movq !%rdx !%rdi
        ),
        data_code1 ++ data_code2, `int
      | Beq, `int, `int | Beq, `bool, `bool ->
        arith_asm text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          sete !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2, `bool
      | Bneq, `int, `int | Bneq, `bool, `bool ->
        arith_asm text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          setne !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2, `bool
      | Blt, `int, `int | Blt, `bool, `bool ->
        arith_asm text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          setl !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2, `bool
      | Ble, `int, `int | Ble, `bool, `bool ->
        arith_asm text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          setle !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2, `bool
      | Bgt, `int, `int | Bgt, `bool, `bool ->
        arith_asm text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          setg !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2, `bool
      | Bge, `int, `int | Bge, `bool, `bool ->
        arith_asm text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          setge !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2, `bool
      | _ ->
        call "runtime_error", nop, `none
      end
    end
  | TEunop (op, e) ->
    begin match op with
    | Uneg ->
      let text_code, data_code, expr_type = compile_expr env e in
      begin match expr_type with
      | `int ->
        text_code ++
        movq (ind ~ofs:(byte) rax) !%rdi ++
        negq !%rdi ++
        pushq !%rdi ++
        movq (imm byte) !%rdi ++
        call "malloc_wrapper" ++
        popq rdi ++
        movq (imm 2) (ind rax) ++
        movq !%rdi (ind ~ofs:(byte) rax),
        data_code, `int
      | _ ->
        failwith "Unsupported Uneg"
      end
    | Unot ->
      let text_code, data_code, expr_type = compile_expr env e in
      begin match expr_type with
      | `bool ->
        text_code ++
        movq (ind ~ofs:(byte) rax) !%rdi ++
        xorq (imm 1) !%rdi ++
        pushq !%rdi ++
        movq (imm byte) !%rdi ++
        call "malloc_wrapper" ++
        popq rdi ++
        movq (imm 1) (ind rax) ++
        movq !%rdi (ind ~ofs:(byte) rax),
        data_code, `bool
      | _ ->
        failwith "Unsupported Unot"
      end
    end
  | TEcall (fn, args) ->
    if fn.fn_name = "len" then
      match args with
      | [e] ->
        let _, _, arg_type = compile_expr env e in
        begin match arg_type with
        | `string | `list -> 
          failwith "Implement len logic here"
        | `int ->
          call "runtime_error", nop, `int
        | _ -> 
          failwith "TypeError: object of type is not iterable"
        end
      | _ ->
        failwith "TypeError: len() takes exactly one argument"
    else
      failwith "Unsupported function call"
  | TElist l ->
    let len = List.length l in
    List.fold_left (fun (text_acc, data_acc, counter) i ->
      let text_expr, data_expr, _ = compile_expr env i in
      text_acc ++
      pushq !%rax ++ (* save heap address*)
      text_expr ++
      movq !%rax !%rsi ++
      popq rax ++
      movq !%rsi (ind ~ofs:(counter) rax),
      data_acc ++ data_expr, counter + byte
    ) (nop, nop, 2 * byte) l |> fun (text_code, data_code, _) ->
    movq (imm ((len + 2) * byte)) !%rdi ++
    call "malloc_wrapper" ++
    movq (imm 4) (ind rax) ++ (* type *)
    movq (imm len) (ind ~ofs:(byte) rax) ++ (* length *)
    text_code, data_code, `list len
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
    let else_label = unique_label env "else" in
    let end_label = unique_label env "end" in
    text_code_cond ++
    cmpq (imm 0) (ind rax) ++
    je else_label ++
    text_code_s1 ++
    jmp end_label ++
    label else_label ++
    text_code_s2 ++
    label end_label
    , data_code_cond ++ data_code_s1 ++ data_code_s2
  | TSreturn expr ->
    failwith "Unsupported Sreturn"
  | TSassign (var, expr) ->
    env.stack_offset <- env.stack_offset - byte;
    let text_code, data_code, expr_type = compile_expr env expr in
    env.vars <- StringMap.add var.v_name (var, env.stack_offset, expr_type) env.vars;
    text_code ++
    movq !%rax (ind ~ofs:(env.stack_offset) rbp)
    , data_code
  | TSprint expr ->
    let text_code, data_code, expr_type = compile_expr env expr in
    begin match expr_type with
    | `none ->
      text_code ++
      call "print_none" ++
      put_character (Char.code '\n'),
      data_code
    | `bool ->
      text_code ++
      movq !%rax !%rdi ++
      call "print_bool" ++
      put_character (Char.code '\n'),
      data_code
    | `int ->
      text_code ++
      movq !%rax !%rdi ++
      call "print_int" ++
      put_character (Char.code '\n'),
      data_code
    | `string _ ->
      text_code ++
      movq !%rax !%rdi ++
      call "print_string" ++
      put_character (Char.code '\n'),
      data_code
    | `list _ ->
      text_code ++
      movq !%rax !%rdi ++
      call "print_list" ++
      put_character (Char.code '\n'),
      data_code
    end
  | TSblock stmts ->
    List.fold_left (fun (acc_text, acc_data) stmt ->
      let text_code, data_code = compile_stmt env stmt in
      acc_text ++ text_code, acc_data ++ data_code
    ) (nop, nop) stmts
    |> fun (text_code, data_code) -> 
      addq (imm (env.stack_offset)) !%rsp ++
      text_code ++
      subq (imm (env.stack_offset)) !%rsp
      , data_code
  | TSfor (var, expr, body) ->
    let text_code_expr, data_code_expr, expr_type = compile_expr env expr in
    begin
      match expr_type with
      | `int | `none | `bool->
        call "runtime_error", nop
      | `list | `string ->
        failwith "Handle supported iterable types here"
    end;
  | TSeval expr ->
    failwith "Unsupported TSeval"
  | TSset (e1, e2, e3) ->
    failwith "Unsupported TSset"

let compile_def env ((fn, body): Ast.tdef) : X86_64.text * X86_64.data =
  let env_local = { env with vars = StringMap.empty; } in
  let prologue = 
    pushq !%rbp ++
    movq !%rsp !%rbp in
  let epilogue = 
    xorq !%rax !%rax ++
    movq !%rbp !%rsp ++
    popq rbp ++
    ret in
  let body_code, data_code = compile_stmt env_local body in
  label fn.fn_name ++ prologue ++ body_code ++ epilogue, data_code


let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  let env = empty_env in
  let runtime_error_text, runtime_error_data = emit_runtime_error env in
  (* Compile each function *)
  let text_code, data_code = List.fold_left (fun (text_acc, data_acc) (fn, body) ->
    let fn_code, data_code = compile_def env (fn, body) in
    (text_acc ++ fn_code, data_acc ++ data_code)
  ) (nop, nop) p in
  { 
    text = 
      c_standard_function_wrapper "malloc" ++
      c_standard_function_wrapper "putchar" ++
      c_standard_function_wrapper "printf" ++
      c_standard_function_wrapper "strcmp" ++
      c_standard_function_wrapper "strcpy" ++
      c_standard_function_wrapper "strcat" ++
      func_print_none_text ++
      func_print_bool_text env ++
      func_print_int_text ++
      func_print_string env ++
      func_print_list env ++
      runtime_error_text ++
      func_copy_two_byte ++
      func_copy_string ++
      func_copy_value env ++
      func_list_concat env ++
      globl "main" ++ text_code;    
    data = 
      data_code ++
      func_print_none_data ++
      func_print_bool_data ++
      func_print_int_data ++
      runtime_error_data;
  }