
open Format
open X86_64
open Ast
open Utils
open Environment
open Functions

let debug = ref false

let rec compile_expr (env: env_t) (parent_env:env_t) (expr: Ast.texpr) : X86_64.text * X86_64.data * ty =
  match expr with
  | TEcst c ->
    begin match c with
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
    end
  | TEvar v ->
    let var, ofs, var_type = StringMap.find v.v_name env.vars in
    movq (ind ~ofs:(ofs+parent_env.stack_offset) rbp) !%rax, nop, var_type
  | TEbinop (op, e1, e2) ->
    begin match op with
    | Band ->
      let text_code1, data_code1, _ = compile_expr env parent_env e1 in
      let text_code2, data_code2, _ = compile_expr env parent_env e2 in
      let l = unique_label env "cond_false" in
      text_code1 ++
      movq (ind ~ofs:(byte) rax) !%rdi ++
      cmpq (imm 0) !%rdi ++
      je l ++
      text_code2 ++
      label l, 
      data_code1 ++ data_code2, `bool  
    | Bor ->
      let text_code1, data_code1, expr_type1 = compile_expr env parent_env e1 in
      let text_code2, data_code2, expr_type2 = compile_expr env parent_env e2 in
      let l = unique_label env "cond_true" in
      text_code1 ++
      movq (ind ~ofs:(byte) rax) !%rdi ++
      cmpq (imm 0) !%rdi ++
      jne l ++
      text_code2 ++
      label l,
      data_code1 ++ data_code2, `bool
    | Badd | Bsub | Bmul | Bdiv | Bmod | Beq | Bneq | Blt | Ble | Bgt | Bge ->
      let text_code1, data_code1, expr_type1 = compile_expr env parent_env e1 in
      let text_code2, data_code2, expr_type2 = compile_expr env parent_env e2 in
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
      | Badd, `list (len1, tl1), `list (len2, tl2) ->
        text_code1 ++
        movq !%rax !%rdi ++
        pushq !%rdi ++
        text_code2 ++
        movq !%rax !%rsi ++
        popq rdi ++
        call "concat_list", data_code1 ++ data_code2, `list (len1 + len2, tl1 @ tl2)
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
      let text_code, data_code, expr_type = compile_expr env parent_env e in
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
      let text_code, data_code, expr_type = compile_expr env parent_env e in
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
        let _, _, arg_type = compile_expr env parent_env e in
        begin match arg_type with
        | `string _ | `list _ -> 
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
    List.fold_left (fun (text_acc, data_acc, counter, type_acc) i ->
      let text_expr, data_expr, expr_type = compile_expr env parent_env i in
      text_acc ++
      pushq !%rax ++ (* save heap address*)
      text_expr ++
      movq !%rax !%rsi ++
      popq rax ++
      movq !%rsi (ind ~ofs:(counter) rax),
      data_acc ++ data_expr, counter + byte, type_acc @ [expr_type]
    ) (nop, nop, 2 * byte, []) l |> fun (text_code, data_code, _, types) ->
    movq (imm ((len + 2) * byte)) !%rdi ++
    call "malloc_wrapper" ++
    movq (imm 4) (ind rax) ++ (* type *)
    movq (imm len) (ind ~ofs:(byte) rax) ++ (* length *)
    text_code, data_code, `list (len, types)
  | TErange e ->
    failwith "Unsupported TErange"
  | TEget (e1, e2) ->
    failwith "Unsupported TEget"

let rec compile_stmt (env: env_t) (parent_env:env_t) (stmt: Ast.tstmt) : X86_64.text * X86_64.data =
  match stmt with
  | TSif (cond, s1, s2) ->
    let text_code_cond, data_code_cond, _ = compile_expr env parent_env cond in
    let text_code_s1, data_code_s1 = compile_stmt env parent_env s1 in
    let text_code_s2, data_code_s2 = compile_stmt env parent_env s2 in
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
    let text_code, data_code, expr_type = compile_expr env parent_env expr in
    env.vars <- StringMap.add var.v_name (var, env.stack_offset, expr_type) env.vars;
    text_code ++
    movq !%rax (ind ~ofs:(env.stack_offset) rbp)
    , data_code
  | TSprint expr ->
    let text_code, data_code, _ = compile_expr env parent_env expr in
    text_code ++
    movq !%rax !%rdi ++
    call "print_value" ++
    put_character '\n',
    data_code
  | TSblock stmts ->
    List.fold_left (fun (acc_text, acc_data) stmt ->
      let text_code, data_code = compile_stmt env parent_env stmt in
      acc_text ++ text_code, acc_data ++ data_code
    ) (nop, nop) stmts
    |> fun (text_code, data_code) -> 
      text_code
      , data_code
  | TSfor (var, expr, body) ->
    let expr_text_code, expr_data_code, expr_type = compile_expr env parent_env expr in
    begin match expr_type with
    | `list (len, tl) ->
      let env_local = {
        vars = StringMap.add var.v_name (var, -byte, `chaos) env.vars;
        funcs = StringMap.empty;
        stack_offset = -byte;
        counters = StringMap.empty;
      } in
      let body_text_code, body_data_code = compile_stmt env_local env body in
      let loop_label = unique_label env "loop" in
      let end_label = unique_label env "end" in
        comment "for loop" ++
        expr_text_code ++
        movq !%rax !%rdi ++
        addq (imm (2 * byte)) !%rdi ++
        addq (imm env_local.stack_offset) !%rsp ++
        movq !%rdi !%rsi ++
        movq (ind rdi) !%r10 ++
        movq !%r10 (ind ~ofs:(env_local.stack_offset+env.stack_offset) rbp) ++
        movq (imm len) !%rcx ++
        
        label loop_label ++
        testq !%rcx !%rcx ++
        jz end_label ++
        movq (ind ~ofs:(env_local.stack_offset+env.stack_offset) rbp) !%rax ++
        pushq !%rcx ++
        pushq !%rdi ++
        pushq !%rsi ++
        comment "body" ++
        body_text_code ++
        comment "end body" ++
        popq rsi ++
        popq rdi ++
        popq rcx ++
        decq !%rcx ++
        addq (imm byte) !%rsi ++
        movq (ind rsi) !%r10 ++
        movq !%r10 (ind ~ofs:(env_local.stack_offset+env.stack_offset) rbp) ++
        jmp loop_label ++
        label end_label ++
        subq (imm env_local.stack_offset) !%rsp
        ,expr_data_code ++ body_data_code
    | _ ->
      call "runtime_error", nop
    end
  | TSeval expr ->
    failwith "Unsupported TSeval"
  | TSset (e1, e2, e3) ->
    failwith "Unsupported TSset"

let compile_def env ((fn, body): Ast.tdef) : X86_64.text * X86_64.data =
  let env_global = {
    vars = StringMap.empty;
    funcs = StringMap.empty;
    stack_offset = 0;
    counters = StringMap.empty;
  } in
  let body_code, data_code = compile_stmt env env_global body in
  let prologue = 
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    addq (imm (env.stack_offset)) !%rsp
  in
  let epilogue = 
    subq (imm (env.stack_offset)) !%rsp ++
    xorq !%rax !%rax ++
    movq !%rbp !%rsp ++
    popq rbp ++
    ret in
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
      runtime_error_text ++
      func_copy_two_byte ++
      func_copy_string ++
      func_copy_value env ++
      func_list_concat env ++
      func_print_value env ++
      globl "main" ++ text_code;    
    data = 
      data_code ++
      func_print_none_data ++
      func_print_bool_data ++
      func_print_int_data ++
      runtime_error_data ++
      inline "\t.section .note.GNU-stack,\"\",@progbits\n";
  }