(** {0 Library for writing X86-64 programs}

    This is only a relatively small fragment
    of the X86-64 assembler.

    @author Jean-Christophe Filliâtre (CNRS)
    @author Kim Nguyen (Université Paris Sud) *)

(** {1 Code} *)

(** abstract type of assembler code.
    The parameter ['a] is used as a phantom type. *)
type 'a asm

(** assembler code located in the text section *)
type text = [ `text ] asm

(** assembler code located in the data section *)
type data = [ `data ] asm

(** address labels are strings *)
type label = string

(** the empty instruction. Can be found in text or data *)
val nop : [> ] asm

(** concatenates two pieces of code (either text with text, or data with data) *)
val ( ++ ) : ([< `text | `data ] asm as 'a) -> 'a -> 'a

(** [inline s] copies the string [s] as is into the assembler file *)
val inline : string -> [> ] asm

(** a program consists of a text section and a data section *)
type program =
  { text : text
  ; data : data
  }

(** [print_program fmt p] prints the code of the program [p] in the
    formatter [fmt] *)
val print_program : Format.formatter -> program -> unit

val print_in_file : file:string -> program -> unit

(** {1 Registers} *)

type size =
  [ `B
  | `W
  | `L
  | `Q
  ]

(** abstract type of registers *)
type 'size register

val rax : [ `Q ] register
val rbx : [ `Q ] register
val rcx : [ `Q ] register
val rdx : [ `Q ] register
val rsi : [ `Q ] register
val rdi : [ `Q ] register
val rbp : [ `Q ] register
val rsp : [ `Q ] register
val r8 : [ `Q ] register
val r9 : [ `Q ] register
val r10 : [ `Q ] register
val r11 : [ `Q ] register
val r12 : [ `Q ] register
val r13 : [ `Q ] register
val r14 : [ `Q ] register

(** 64-bit registers *)
val r15 : [ `Q ] register

val eax : [ `L ] register
val ebx : [ `L ] register
val ecx : [ `L ] register
val edx : [ `L ] register
val esi : [ `L ] register
val edi : [ `L ] register
val ebp : [ `L ] register
val esp : [ `L ] register
val r8d : [ `L ] register
val r9d : [ `L ] register
val r10d : [ `L ] register
val r11d : [ `L ] register
val r12d : [ `L ] register
val r13d : [ `L ] register
val r14d : [ `L ] register

(** 32-bit registers *)
val r15d : [ `L ] register

val ax : [ `W ] register
val bx : [ `W ] register
val cx : [ `W ] register
val dx : [ `W ] register
val si : [ `W ] register
val di : [ `W ] register
val bp : [ `W ] register
val sp : [ `W ] register
val r8w : [ `W ] register
val r9w : [ `W ] register
val r10w : [ `W ] register
val r11w : [ `W ] register
val r12w : [ `W ] register
val r13w : [ `W ] register
val r14w : [ `W ] register

(** 16-bit registers *)
val r15w : [ `W ] register

val al : [ `B ] register
val bl : [ `B ] register
val cl : [ `B ] register
val dl : [ `B ] register
val ah : [ `B ] register
val bh : [ `B ] register
val ch : [ `B ] register
val dh : [ `B ] register
val sil : [ `B ] register
val dil : [ `B ] register
val bpl : [ `B ] register
val spl : [ `B ] register
val r8b : [ `B ] register
val r9b : [ `B ] register
val r10b : [ `B ] register
val r11b : [ `B ] register
val r12b : [ `B ] register
val r13b : [ `B ] register
val r14b : [ `B ] register

(** 8-bit registers *)
val r15b : [ `B ] register

(** {1 Operands} *)

(** The abstract type of operands *)
type 'size operand

(** immediate operand $i *)
val imm : int -> [> ] operand

(** immediate operand $i *)
val imm32 : int32 -> [> ] operand

(** immediate operand $i *)
val imm64 : int64 -> [> ] operand

val reg : 'size register -> 'size operand

(** register *)
val ( !% ) : 'size register -> 'size operand

(** indirect operand ofs(register, index, scale) *)
val ind
  :  ?ofs:int
  -> ?index:'size1 register
  -> ?scale:int
  -> 'size2 register
  -> [> ] operand

(** label L *)
val lab : label -> [> ] operand

(** immediate label $L *)
val ilab : label -> [ `Q ] operand

(** {1 Instructions} *)

(** {2 Transfer} *)

val movb : [ `B ] operand -> [ `B ] operand -> text
val movw : [ `W ] operand -> [ `W ] operand -> text
val movl : [ `L ] operand -> [ `L ] operand -> text

(** Note: not all operand combinations are allowed *)
val movq : [ `Q ] operand -> [ `Q ] operand -> text

val movsbw : [ `B ] operand -> [ `W ] register -> text
val movsbl : [ `B ] operand -> [ `L ] register -> text
val movsbq : [ `B ] operand -> [ `Q ] register -> text
val movswl : [ `W ] operand -> [ `L ] register -> text
val movswq : [ `W ] operand -> [ `Q ] register -> text

(** with sign extension *)
val movslq : [ `L ] operand -> [ `Q ] register -> text

val movzbw : [ `B ] operand -> [ `W ] register -> text
val movzbl : [ `B ] operand -> [ `L ] register -> text
val movzbq : [ `B ] operand -> [ `Q ] register -> text
val movzwl : [ `W ] operand -> [ `L ] register -> text

(** with zero extension *)
val movzwq : [ `W ] operand -> [ `Q ] register -> text

(** copies a 64-bit immediate value into a register *)
val movabsq : [ `Q ] operand -> [ `Q ] register -> text

val cmove : 'size operand -> 'size operand -> text (* =  0 *)
val cmovz : 'size operand -> 'size operand -> text (* =  0 *)
val cmovne : 'size operand -> 'size operand -> text (* <> 0 *)
val cmovnz : 'size operand -> 'size operand -> text (* <> 0 *)
val cmovs : 'size operand -> 'size operand -> text (* <  0 *)
val cmovns : 'size operand -> 'size operand -> text (* >= 0 *)
val cmovg : 'size operand -> 'size operand -> text (* >  signed *)
val cmovge : 'size operand -> 'size operand -> text (* >= signed *)
val cmovl : 'size operand -> 'size operand -> text (* <  signed *)
val cmovle : 'size operand -> 'size operand -> text (* <= signed *)
val cmova : 'size operand -> 'size operand -> text (* >  unsigned *)
val cmovae : 'size operand -> 'size operand -> text (* >= unsigned *)
val cmovb : 'size operand -> 'size operand -> text (* <  unsigned *)

(** conditional move
    (note: not all operand combinations are allowed) *)
val cmovbe : 'size operand -> 'size operand -> text (* <= unsigned *)

(** {2 Arithmetic} *)

val leab : [ `B ] operand -> [ `B ] register -> text
val leaw : [ `W ] operand -> [ `W ] register -> text
val leal : [ `L ] operand -> [ `L ] register -> text
val leaq : [ `Q ] operand -> [ `Q ] register -> text
val incb : [ `B ] operand -> text
val incw : [ `W ] operand -> text
val incl : [ `L ] operand -> text
val incq : [ `Q ] operand -> text
val decb : [ `B ] operand -> text
val decw : [ `W ] operand -> text
val decl : [ `L ] operand -> text
val decq : [ `Q ] operand -> text
val negb : [ `B ] operand -> text
val negw : [ `W ] operand -> text
val negl : [ `L ] operand -> text
val negq : [ `Q ] operand -> text
val addb : [ `B ] operand -> [ `B ] operand -> text
val addw : [ `W ] operand -> [ `W ] operand -> text
val addl : [ `L ] operand -> [ `L ] operand -> text
val addq : [ `Q ] operand -> [ `Q ] operand -> text
val subb : [ `B ] operand -> [ `B ] operand -> text
val subw : [ `W ] operand -> [ `W ] operand -> text
val subl : [ `L ] operand -> [ `L ] operand -> text
val subq : [ `Q ] operand -> [ `Q ] operand -> text
val imulw : [ `W ] operand -> [ `W ] operand -> text
val imull : [ `L ] operand -> [ `L ] operand -> text
val imulq : [ `Q ] operand -> [ `Q ] operand -> text
val idivq : [ `Q ] operand -> text
val cqto : text

(** {2 Logical operations} *)

val notb : [ `B ] operand -> text
val notw : [ `W ] operand -> text
val notl : [ `L ] operand -> text
val notq : [ `Q ] operand -> text
val andb : [ `B ] operand -> [ `B ] operand -> text
val andw : [ `W ] operand -> [ `W ] operand -> text
val andl : [ `L ] operand -> [ `L ] operand -> text
val andq : [ `Q ] operand -> [ `Q ] operand -> text
val orb : [ `B ] operand -> [ `B ] operand -> text
val orw : [ `W ] operand -> [ `W ] operand -> text
val orl : [ `L ] operand -> [ `L ] operand -> text
val orq : [ `Q ] operand -> [ `Q ] operand -> text
val xorb : [ `B ] operand -> [ `B ] operand -> text
val xorw : [ `W ] operand -> [ `W ] operand -> text
val xorl : [ `L ] operand -> [ `L ] operand -> text

(** Bit manipulation operations. Bitwise "and", bitwise "or", bitwise "not" *)
val xorq : [ `Q ] operand -> [ `Q ] operand -> text

(** {2 Shifts} *)

val shlb : [ `B ] operand -> [ `B ] operand -> text
val shlw : [ `W ] operand -> [ `W ] operand -> text
val shll : [ `L ] operand -> [ `L ] operand -> text

(** note: shl is the same as sal *)
val shlq : [ `Q ] operand -> [ `Q ] operand -> text

val shrb : [ `B ] operand -> [ `B ] operand -> text
val shrw : [ `W ] operand -> [ `W ] operand -> text
val shrl : [ `L ] operand -> [ `L ] operand -> text
val shrq : [ `Q ] operand -> [ `Q ] operand -> text
val sarb : [ `B ] operand -> [ `B ] operand -> text
val sarw : [ `W ] operand -> [ `W ] operand -> text
val sarl : [ `L ] operand -> [ `L ] operand -> text
val sarq : [ `Q ] operand -> [ `Q ] operand -> text

(** {2 Jumps} *)

val call : label -> text
val call_star : [ `Q ] operand -> text
val leave : text

(** function call and return *)
val ret : text

(** unconditional jump *)
val jmp : label -> text

(** jump to a computed address *)
val jmp_star : [ `Q ] operand -> text

val je : label -> text (* =  0 *)
val jz : label -> text (* =  0 *)
val jne : label -> text (* <> 0 *)
val jnz : label -> text (* <> 0 *)
val js : label -> text (* <  0 *)
val jns : label -> text (* >= 0 *)
val jg : label -> text (* >  signed *)
val jge : label -> text (* >= signed *)
val jl : label -> text (* <  signed *)
val jle : label -> text (* <= signed *)
val ja : label -> text (* >  unsigned *)
val jae : label -> text (* >= unsigned *)
val jb : label -> text (* <  unsigned *)

(** conditional jumps *)
val jbe : label -> text (* <= unsigned *)

(** {2 Conditions} *)

val cmpb : [ `B ] operand -> [ `B ] operand -> text
val cmpw : [ `W ] operand -> [ `W ] operand -> text
val cmpl : [ `L ] operand -> [ `L ] operand -> text
val cmpq : [ `Q ] operand -> [ `Q ] operand -> text
val testb : [ `B ] operand -> [ `B ] operand -> text
val testw : [ `W ] operand -> [ `W ] operand -> text
val testl : [ `L ] operand -> [ `L ] operand -> text
val testq : [ `Q ] operand -> [ `Q ] operand -> text
val sete : [ `B ] operand -> text (* =  0 *)
val setne : [ `B ] operand -> text (* <> 0 *)
val sets : [ `B ] operand -> text (* <  0 *)
val setns : [ `B ] operand -> text (* >= 0 *)
val setg : [ `B ] operand -> text (* >  signed *)
val setge : [ `B ] operand -> text (* >= signed *)
val setl : [ `B ] operand -> text (* <  signed *)
val setle : [ `B ] operand -> text (* <= signed *)
val seta : [ `B ] operand -> text (* >  unsigned *)
val setae : [ `B ] operand -> text (* >= unsigned *)
val setb : [ `B ] operand -> text (* <  unsigned *)

(** sets the operand byte to 1 or 0 depending on whether the test is true or not *)
val setbe : [ `B ] operand -> text (* <= unsigned *)

(** {2 Stack manipulation} *)

(** [pushq r] places the content of [r] at the top of the stack.
    Reminder: %rsp points to the address of the last occupied slot *)
val pushq : [ `Q ] operand -> text

(** [popq r] places the word at the top of the stack into [r] and pops the stack *)
val popq : [ `Q ] register -> text

(** {2 Misc.} *)

(** a label. Can be found in text or data *)
val label : label -> [> ] asm

(** .globl declaration (typically for main) *)
val globl : label -> [> ] asm

(** places a comment in the generated code. Can be found in
    text or data *)
val comment : string -> [> ] asm

(** {2 Data} *)

(** a string constant (terminated by 0) *)
val string : string -> data

val dbyte : int list -> data
val dword : int list -> data
val dint : int list -> data

(** place a list of values on 1/2/4/8 bytes in the data section *)
val dquad : int list -> data

(** place a list of addresses in the data section (with .quad) *)
val address : label list -> data

(** [space n] allocates [n] bytes (set to 0) in the data segment *)
val space : int -> data
