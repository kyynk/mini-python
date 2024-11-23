
open Format
open X86_64
open Ast

let debug = ref false

let dprintf fmt =
  if !debug then
    printf fmt
  else
    ifprintf std_formatter fmt

let rec compile_expr (e : Ast.expr) : X86_64.text =
  match e with
  | Ecst c -> 
    begin
      match c with
      | Cnone -> nop
      | Cbool b -> nop
      | Cstring s -> nop
      | Cint i -> nop
    end
  | Eident id -> nop
  | Ebinop (op, e1, e2) -> nop
  | Eunop (op, e) -> nop
  | Ecall (id, args) -> nop
  | Elist el -> nop
  | Eget (e1, e2) -> nop

let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  { text = globl "main" ++ label "main" ++ ret;   (* TODO *)
    data = nop; }                                 (* TODO *)
