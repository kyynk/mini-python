open Environment
open X86_64
open Utils

let func_print_none : X86_64.text =
  label "print_none" ++
  asm_print_none ++
  ret

let func_print_bool (env:env_t): X86_64.text =
  let func_print_bool_false = unique_label env "func_print_bool_false" in
  let func_print_bool_end = unique_label env "func_print_bool_end" in
  label "print_bool" ++
  asm_print_bool func_print_bool_false func_print_bool_end ++
  ret

let func_print_int : X86_64.text =
  label "print_int" ++
  asm_print_int ++
  ret

let func_print_string (env:env_t): X86_64.text =
  let func_print_string_start = unique_label env "func_print_string_start" in
  let func_print_string_end = unique_label env "func_print_string_end" in
  label "print_string" ++
  asm_print_string func_print_string_start func_print_string_end ++
  ret

let func_print_list (env:env_t): X86_64.text =
  let list_start = unique_label env "list_start" in
  let list_loop_start = unique_label env "list_loop_start" in
  let list_loop_end = unique_label env "list_loop_end" in
  let list_loop_first_time = unique_label env "list_loop_first_time" in
  let none_case = unique_label env "none_case" in
  let bool_case = unique_label env "bool_case" in
  let bool_label_false = unique_label env "bool_label_false" in
  let bool_label_end = unique_label env "bool_label_end" in
  let int_case = unique_label env "int_case" in
  let string_case = unique_label env "string_case" in
  let string_loop = unique_label env "string_loop" in
  let string_loop_end = unique_label env "string_loop_end" in
  let list_end = unique_label env "list_end" in
  
  label "print_list" ++
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  subq (imm byte) !%rsp ++
  movq (imm 0) (ind ~ofs:(-byte) rbp) ++
  (*
  rcx: counter, start from length of list
  rsi: saved first pointer element of list
  rdi: current pointer element of list 
   *)
  label list_start ++
  movq (ind ~ofs:(byte) rdi) !%rcx ++
  addq (imm (2 * byte)) !%rdi ++
  movq !%rdi !%rsi ++
  pushq !%rsi ++
  pushq !%rdi ++
  pushq !%rcx ++
  put_character lbrac ++
  popq rcx ++
  popq rdi ++
  popq rsi ++

  label list_loop_start ++
  testq !%rcx !%rcx ++
  jz list_loop_end ++
  cmpq !%rdi !%rsi ++
  je list_loop_first_time ++
  pushq !%rsi ++
  pushq !%rdi ++
  pushq !%rcx ++
  put_character comma ++
  put_character space ++
  popq rcx ++
  popq rdi ++
  popq rsi ++

  label list_loop_first_time ++
  movq (ind rdi) !%rdx ++
  movq (ind rdx) !%rdx ++
  cmpq (imm 0) !%rdx ++
  je none_case ++
  cmpq (imm 1) !%rdx ++
  je bool_case ++
  cmpq (imm 2) !%rdx ++
  je int_case ++
  cmpq (imm 3) !%rdx ++
  je string_case ++
  cmpq (imm 4) !%rdx ++
  jne "runtime_error" ++
  pushq !%rsi ++
  pushq !%rdi ++
  pushq !%rcx ++
  movq (ind ~ofs:(-byte) rbp) !%rax ++
  incq !%rax ++
  movq !%rax (ind ~ofs:(-byte) rbp) ++
  movq (ind rdi) !%rdi ++
  jmp list_start ++


  (* none *)
  label none_case ++
  pushq !%rsi ++
  pushq !%rdi ++
  pushq !%rcx ++
  asm_print_none ++
  popq rcx ++
  popq rdi ++
  popq rsi ++
  decq !%rcx ++
  addq (imm byte) !%rdi ++
  jmp list_loop_start ++


  (* bool *)
  label bool_case ++
  pushq !%rsi ++
  pushq !%rdi ++
  pushq !%rcx ++
  movq (ind rdi) !%rdi ++
  asm_print_bool bool_label_false bool_label_end ++
  popq rcx ++
  popq rdi ++
  popq rsi ++
  decq !%rcx ++
  addq (imm byte) !%rdi ++
  jmp list_loop_start ++


  (* int *)
  label int_case ++
  pushq !%rsi ++
  pushq !%rdi ++
  pushq !%rcx ++
  movq (ind rdi) !%rdi ++
  asm_print_int ++
  popq rcx ++
  popq rdi ++
  popq rsi ++
  decq !%rcx ++
  addq (imm byte) !%rdi ++
  jmp list_loop_start ++


  (* string *)
  label string_case ++
  pushq !%rsi ++
  pushq !%rdi ++
  pushq !%rcx ++
  movq (ind rdi) !%rdi ++
  asm_print_string string_loop string_loop_end ++
  popq rcx ++
  popq rdi ++
  popq rsi ++
  decq !%rcx ++
  addq (imm byte) !%rdi ++
  jmp list_loop_start ++

  label list_loop_end ++
  pushq !%rsi ++
  pushq !%rdi ++
  pushq !%rcx ++
  put_character rbrac ++
  popq rcx ++
  popq rdi ++
  popq rsi ++
  movq (ind ~ofs:(-byte) rbp) !%rax ++
  testq !%rax !%rax ++
  jz list_end ++
  decq !%rax ++
  movq !%rax (ind ~ofs:(-byte) rbp) ++
  popq rcx ++
  popq rdi ++
  popq rsi ++
  decq !%rcx ++
  addq (imm byte) !%rdi ++
  jmp list_loop_start ++

  label list_end ++
  addq (imm byte) !%rsp ++
  movq !%rbp !%rsp ++
  popq rbp ++
  ret