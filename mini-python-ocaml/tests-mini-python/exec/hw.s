	.text
malloc_wrapper:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	xorq %rax, %rax
	call malloc
	movq %rbp, %rsp
	popq %rbp
	ret
putchar_wrapper:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	xorq %rax, %rax
	call putchar
	movq %rbp, %rsp
	popq %rbp
	ret
printf_wrapper:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	xorq %rax, %rax
	call printf
	movq %rbp, %rsp
	popq %rbp
	ret
strcmp_wrapper:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	xorq %rax, %rax
	call strcmp
	movq %rbp, %rsp
	popq %rbp
	ret
strcpy_wrapper:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	xorq %rax, %rax
	call strcpy
	movq %rbp, %rsp
	popq %rbp
	ret
strcat_wrapper:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	xorq %rax, %rax
	call strcat
	movq %rbp, %rsp
	popq %rbp
	ret
runtime_error:
	pushq %rbp
	movq %rsp, %rbp
	leaq runtime_error_msg, %rdi
	xorq %rax, %rax
	call printf
	movq $1, %rdi
	call exit
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	addq $0, %rsp
	movq $120, %rdi
	call malloc_wrapper
	movq $3, 0(%rax)
	movq $12, 8(%rax)
	movq $104, 16(%rax)
	movq $101, 24(%rax)
	movq $108, 32(%rax)
	movq $108, 40(%rax)
	movq $111, 48(%rax)
	movq $44, 56(%rax)
	movq $32, 64(%rax)
	movq $119, 72(%rax)
	movq $111, 80(%rax)
	movq $114, 88(%rax)
	movq $108, 96(%rax)
	movq $100, 104(%rax)
	movq $0, 112(%rax)
	movq %rax, %rsi
loop_start0:
	movq 0(%rsi), %rax
	testq %rax, %rax
	jz loop_end0
	movq %rax, %rdi
	pushq %rsi
	call putchar_wrapper
	popq %rsi
	addq $8, %rsi
	jmp loop_start0
loop_end0:
	movq $10, %rdi
	call putchar_wrapper
	subq $0, %rsp
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
print_int:
	.string "%d\n"
true_string:
	.string "True\n"
false_string:
	.string "False\n"
