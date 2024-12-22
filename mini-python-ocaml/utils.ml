open Format
open X86_64
open Ast
open Environment

let byte = 8

let put_character (c:char): X86_64.text =
  movq (imm (Char.code c)) !%rdi ++
  call "putchar_wrapper"


let unique_label (env: env_t) (prefix: string) : string =
  let counter =
    match StringMap.find_opt prefix env.counters with
    | Some c -> c
    | None -> 0
  in
  env.counters <- StringMap.add prefix (counter + 1) env.counters;
  Printf.sprintf "%s%d" prefix counter


let arith_asm (code1:X86_64.text) (code2:X86_64.text) (instructions:X86_64.text) : X86_64.text =
  code1 ++
  movq (ind ~ofs:(byte) rax) (reg rdi) ++
  pushq (reg rdi) ++
  code2 ++
  popq rdi ++
  movq (ind ~ofs:(byte) rax) (reg rsi) ++
  instructions ++
  pushq (reg rdi) ++
  movq (imm byte) (reg rdi) ++
  call "malloc_wrapper" ++
  popq rdi ++
  movq (imm 2) (ind rax) ++
  movq (reg rdi) (ind ~ofs:(byte) rax)


let c_standard_function_wrapper (fn_name:string): X86_64.text =
  label (fn_name ^ "_wrapper") ++ 
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  andq (imm (-16)) (reg rsp) ++
  call fn_name ++
  movq (reg rbp) (reg rsp) ++
  popq rbp ++
  ret

let emit_runtime_error (env:env_t): X86_64.text * X86_64.data =
  let error_text_label = "runtime_error" in
  let error_data_label = unique_label env "emit_runtime_error_msg" in
  let text_code =
    label error_text_label ++
    leaq (lab error_data_label) rdi ++
    xorq !%rax !%rax ++
    call "printf_wrapper" ++
    movq (imm 1) !%rdi ++
    call "exit" ++
    nop
  in
  let data_code =
    label error_data_label ++
    string "Runtime error occurred\n"
  in
  text_code, data_code


let construct_texpr_list (len:int) : texpr list =
  let rec aux acc i =
    if i = len then acc
    else aux (TEcst (Cint 0L) :: acc) (i + 1)
  in
  aux [] 0
