
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
  mutable counters: int StringMap.t;
}

let empty_env = {
  vars = StringMap.empty;
  funcs = StringMap.empty;
  stack_offset = 0;
  counters = StringMap.empty;
}

let byte = 8

let unique_label (env: env_t) (prefix: string) : string =
  let counter =
    match StringMap.find_opt prefix env.counters with
    | Some c -> c
    | None -> 0
  in
  env.counters <- StringMap.add prefix (counter + 1) env.counters;
  Printf.sprintf "%s%d" prefix counter


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
    let len = String.length s in
    let (text_code, _) = String.fold_left (fun (acc, counter) i ->
      let counter = counter + byte in
      acc ++ movq (imm (Char.code i)) (ind ~ofs:(counter) rax), counter
    ) (nop, byte) s in
    movq (imm ((len + 3) * byte)) (reg rdi) ++
    call "malloc_wrapper" ++
    movq (imm 3) (ind rax) ++
    movq (imm len) (ind ~ofs:(byte) rax) ++
    text_code ++
    movq (imm 0) (ind ~ofs:((len + 2) * byte) rax)
    ,nop , `string

let compile_var (env: env_t) (v: Ast.var) : X86_64.text * X86_64.data * ty =
    let var, ofs, var_type = StringMap.find v.v_name env.vars in
      movq (ind ~ofs:(ofs) rbp) (reg rax), nop, var_type

let arith_asm (code1:X86_64.text) (code2:X86_64.text) (instructions:X86_64.text) : X86_64.text =
  code1 ++
  movq (ind rax) (reg rdi) ++
  pushq (reg rdi) ++
  code2 ++
  popq rdi ++
  movq (ind rax) (reg rsi) ++
  instructions ++
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

let create_runtime_error () : X86_64.text * X86_64.data =
  let error_label = "runtime_error" in
  let error_message = "Runtime error occurred\n" in
  let text_code =
    label error_label ++
    (* Prologue *)
    pushq (reg rbp) ++
    movq (reg rsp) (reg rbp) ++
    (* Load the error message *)
    leaq (lab "runtime_error_msg") rdi ++
    xorq (reg rax) (reg rax) ++
    (* Call printf to print the error message *)
    call "printf" ++
    (* Exit with a non-zero status *)
    movq (imm 1) (reg rdi) ++
    call "exit" ++
    (* No need for epilogue as the process will exit *)
    nop
  in
  let data_code =
    (* Data section for the error message *)
    label "runtime_error_msg" ++
    string error_message
  in
  text_code, data_code

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
      let text_code1, data_code1, expr_type1 = compile_expr env e1 in
      let text_code2, data_code2, expr_type2 = compile_expr env e2 in
      begin
      if expr_type1 <> `bool || expr_type2 <> `bool then failwith "compilation error";
        let l = unique_label env "cond_false" in
        text_code1 ++
        movq (ind ~ofs:(byte) rax) !%rdi ++
        cmpq (imm 0) !%rdi ++
        je l ++
        text_code2 ++
        label l        
        , data_code1 ++ data_code2, `bool  
      end
    | Bor ->
      let text_code1, data_code1, expr_type1 = compile_expr env e1 in
      let text_code2, data_code2, expr_type2 = compile_expr env e2 in
      begin
      if expr_type1 <> `bool || expr_type2 <> `bool then failwith "compilation error";
        let l = unique_label env "cond_true" in
        text_code1 ++
        movq (ind ~ofs:(byte) rax) !%rdi ++
        cmpq (imm 0) !%rdi ++
        jne l ++
        text_code2 ++
        label l
        , data_code1 ++ data_code2, `bool
      end
    | Badd | Bsub | Bmul | Bdiv | Bmod | Beq | Bneq | Blt | Ble | Bgt | Bge ->
      let text_code1, data_code1, expr_type1 = compile_expr env e1 in
      let text_code2, data_code2, expr_type2 = compile_expr env e2 in
      begin match op, expr_type1, expr_type2 with
      | Badd, `int, `int ->
        arith_asm text_code1 text_code2 (addq (reg rsi) (reg rdi)),
        data_code1 ++ data_code2, `int
      | Badd, `int, `string ->
        print_endline "Badd int string";
        text_code1 ++ text_code2 ++ call "runtime_error"
        , nop, `int
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
      pushq (reg rax) ++ (* save heap address*)
      text_code ++
      movq (reg rax) (reg rsi) ++
      popq rax ++
      movq (reg rsi) (ind ~ofs:(counter) rax),
      counter + byte
    ) (nop, 2 * byte) l |> fun (text_code, _) ->
    movq (imm ((len + 2) * byte)) (reg rdi) ++
    call "malloc_wrapper" ++
    movq (imm 4) (ind rax) ++ (* type *)
    movq (imm len) (ind ~ofs:(byte) rax) ++ (* length *)
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
    movq (reg rax) (ind ~ofs:(env.stack_offset) rbp)
    , data_code
  | TSprint expr ->
    let text_code, data_code, expr_type = compile_expr env expr in
    begin match expr_type with
    | `none ->
      text_code ++
      leaq (lab "none_string") rdi ++
      call "printf_wrapper",
      data_code
    | `int ->
      text_code ++
      movq (ind ~ofs:(byte) rax) (reg rdi) ++
      leaq (lab "print_int") rdi ++
      call "printf_wrapper",
      data_code
    | `string ->
      let loop_start = unique_label env "loop_start" in
      let loop_end = unique_label env "loop_end" in
      text_code ++
      movq (reg rax) (reg rsi) ++
      (* pointer in rsi *)
      label loop_start ++
      movq (ind ~ofs:(byte) rsi) (reg rax) ++
      testq (reg rax) (reg rax) ++
      jz loop_end ++
      movq (reg rax) (reg rdi) ++
      pushq (reg rsi) ++
      call "putchar_wrapper" ++
      popq rsi ++
      addq (imm byte) (reg rsi) ++
      jmp loop_start ++
      label loop_end ++
      (* \n *)
      movq (imm 10) (reg rdi) ++
      call "putchar_wrapper"
      , data_code
    | `bool ->
      let b_false = unique_label env "b_false" in
      let b_end = unique_label env "b_end" in
      text_code ++
      movq (ind ~ofs:(byte) rax) !%rsi ++
      testq !%rsi !%rsi ++
      jz b_false ++
      leaq (lab "true_string") rdi ++
      call "printf_wrapper" ++
      jmp b_end ++
      label b_false ++
      leaq (lab "false_string") rdi ++
      call "printf_wrapper" ++
      label b_end,
      data_code
    | `list ->
      let loop_start = unique_label env "loop_start" in
      let loop_end = unique_label env "loop_end" in
      let to_none = unique_label env "to_none" in
      let to_bool = unique_label env "to_bool" in
      let b_false = unique_label env "b_false" in
      let to_int = unique_label env "to_int" in
      let to_string = unique_label env "to_string" in
      let loop_string = unique_label env "loop_string" in
      let loop_string_end = unique_label env "loop_string_end" in
      text_code ++
      movq (ind ~ofs:(byte) rax) !%rdi ++ (* length *)
      addq (imm (2*byte)) !%rax ++
      movq !%rax !%rsi ++ (* first element *)
      label loop_start ++
      cmpq (imm 0) !%rdi ++
      je loop_end ++
      movq (ind rsi) !%rdx ++
      cmpq (imm 0) !%rdx ++
      je to_none ++
      cmpq (imm 1) !%rdx ++
      je to_bool ++
      cmpq (imm 2) !%rdx ++
      je to_int ++
      cmpq (imm 3) !%rdx ++
      je to_string ++
      jmp loop_end ++
      label to_none ++
      (* none *)
      pushq !%rsi ++
      pushq !%rdi ++

      leaq (lab "none_string") rdi ++
      call "printf_wrapper" ++
      
      popq rdi ++
      popq rsi ++
      jmp loop_end ++
      
      (* bool *)
      label to_bool ++
      pushq !%rsi ++
      pushq !%rdi ++
      movq (ind ~ofs:(byte) rsi) !%rdi ++
      cmpq (imm 0) !%rdi ++
      je b_false ++
      leaq (lab "true_string") rdi ++
      call "printf_wrapper" ++
      popq rdi ++
      popq rsi ++
      jmp loop_end ++
      label b_false ++
      leaq (lab "false_string") rdi ++
      call "printf_wrapper" ++
      popq rdi ++
      popq rsi ++
      jmp loop_end ++
      
      (* int *)
      label to_int ++
      pushq !%rsi ++
      pushq !%rdi ++
      movq (ind ~ofs:(byte) rsi) !%rdi ++
      leaq (lab "print_int") rdi ++
      call "printf_wrapper" ++
      popq rdi ++
      popq rsi ++
      jmp loop_end ++
      
      (* string *)
      label to_string ++
      pushq !%rsi ++
      pushq !%rdi ++


      

      , nop
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
  let runtime_error_text, runtime_error_data = create_runtime_error () in
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
      runtime_error_text ++
      globl "main" ++ text_code;
    data = 
      data_code ++
      runtime_error_data ++
      label "print_int" ++
      string "%d\n" ++
      label "true_string" ++
      string "True\n" ++
      label "false_string" ++
	    string "False\n" ++
      label "none_string" ++
      string "None\n";
      (* hard-coded print_int *)
  }