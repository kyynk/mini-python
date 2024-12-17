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
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	addq $-8, %rsp
	movq $48, %rdi
	call malloc_wrapper
	movq $4, 0(%rax)
	movq $97, 16(%rax)
	movq $115, 24(%rax)
	movq $100, 32(%rax)
	movq $102, 40(%rax)
	movq $0, 48(%rax)
	movq %rax, -8(%rbp)
	movq -8(%rbp), %rax
	// movq %rax, %rsi
// loop_start:
	// movq 8(%rsi), %rax
	// testq %rax, %rax
	// jz loop_end
	movq 16(%rax), %rdi
	pushq %rsi
	call putchar_wrapper
	popq %rsi
	addq $8, %rsi
	// jmp loop_start
// loop_end:
	subq $-8, %rsp
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
print_int:
	.string "%d\n"
print_str:
	.string "%s\n"
true_string:
	.string "True"
false_string:
	.string "False"
