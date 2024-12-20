open Environment
open X86_64
open Utils

let func_print_none : X86_64.text =
  label "print_none" ++
  Utils.asm_print_none ++
  ret

let func_print_bool (env:env_t): X86_64.text =
  let func_print_bool_false = unique_label env "func_print_bool_false" in
  let func_print_bool_end = unique_label env "func_print_bool_end" in
  label "print_bool" ++
  Utils.asm_print_bool func_print_bool_false func_print_bool_end ++
  ret

let func_print_int : X86_64.text =
  label "print_int" ++
  asm_print_int ++
  ret

let func_print_string (env:env_t): X86_64.text =
  let func_print_string_start = unique_label env "func_print_string_start" in
  let func_print_string_end = unique_label env "func_print_string_end" in
  label "print_string" ++
  Utils.asm_print_string func_print_string_start func_print_string_end ++
  ret


let func_print_list_save_reg : X86_64.text =
  pushq !%rsi ++
  pushq !%rdi ++
  pushq !%rcx

let func_print_list_restore_reg : X86_64.text =
  popq rcx ++
  popq rdi ++
  popq rsi

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
  func_print_list_save_reg ++
  put_character (Char.code '[') ++
  func_print_list_restore_reg ++

  label list_loop_start ++
  testq !%rcx !%rcx ++
  jz list_loop_end ++
  cmpq !%rdi !%rsi ++
  je list_loop_first_time ++
  func_print_list_save_reg ++
  put_character (Char.code ',') ++
  put_character (Char.code ' ') ++
  func_print_list_restore_reg ++

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
  func_print_list_save_reg ++
  movq (ind ~ofs:(-byte) rbp) !%rax ++
  incq !%rax ++
  movq !%rax (ind ~ofs:(-byte) rbp) ++
  movq (ind rdi) !%rdi ++
  jmp list_start ++


  (* none *)
  label none_case ++
  func_print_list_save_reg ++
  Utils.asm_print_none ++
  func_print_list_restore_reg ++
  decq !%rcx ++
  addq (imm byte) !%rdi ++
  jmp list_loop_start ++


  (* bool *)
  label bool_case ++
  func_print_list_save_reg ++
  movq (ind rdi) !%rdi ++
  Utils.asm_print_bool bool_label_false bool_label_end ++
  func_print_list_restore_reg ++
  decq !%rcx ++
  addq (imm byte) !%rdi ++
  jmp list_loop_start ++


  (* int *)
  label int_case ++
  func_print_list_save_reg ++
  movq (ind rdi) !%rdi ++
  Utils.asm_print_int ++
  func_print_list_restore_reg ++
  decq !%rcx ++
  addq (imm byte) !%rdi ++
  jmp list_loop_start ++


  (* string *)
  label string_case ++
  func_print_list_save_reg ++
  movq (ind rdi) !%rdi ++
  Utils.asm_print_string string_loop string_loop_end ++
  func_print_list_restore_reg ++
  decq !%rcx ++
  addq (imm byte) !%rdi ++
  jmp list_loop_start ++

  label list_loop_end ++
  func_print_list_save_reg ++
  put_character (Char.code ']') ++
  func_print_list_restore_reg ++
  movq (ind ~ofs:(-byte) rbp) !%rax ++
  testq !%rax !%rax ++
  jz list_end ++
  decq !%rax ++
  movq !%rax (ind ~ofs:(-byte) rbp) ++
  func_print_list_restore_reg ++
  decq !%rcx ++
  addq (imm byte) !%rdi ++
  jmp list_loop_start ++

  label list_end ++
  addq (imm byte) !%rsp ++
  movq !%rbp !%rsp ++
  popq rbp ++
  ret

