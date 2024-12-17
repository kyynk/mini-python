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
	addq $-48, %rsp
	movq $56, %rdi
	call malloc_wrapper
	movq $3, 0(%rax)
	movq $4, 8(%rax)
	movq $97, 16(%rax)
	movq $115, 24(%rax)
	movq $100, 32(%rax)
	movq $102, 40(%rax)
	movq $0, 48(%rax)
	movq %rax, -8(%rbp)
	movq $16, %rdi
	call malloc_wrapper
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	movq %rax, -16(%rbp)
	movq $16, %rdi
	call malloc_wrapper
	movq $1, 0(%rax)
	movq $1, 8(%rax)
	movq %rax, -24(%rbp)
	movq $16, %rdi
	call malloc_wrapper
	movq $1, 0(%rax)
	movq $0, 8(%rax)
	movq %rax, -32(%rbp)
	movq -8(%rbp), %rax
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
	movq -16(%rbp), %rax
	movq 8(%rax), %rdi
	leaq print_int, %rdi
	call printf_wrapper
	movq -24(%rbp), %rax
	movq 8(%rax), %rsi
	testq %rsi, %rsi
	jz b_false0
	leaq true_string, %rdi
	call printf_wrapper
	jmp b_end0
b_false0:
	leaq false_string, %rdi
	call printf_wrapper
b_end0:
	movq -24(%rbp), %rax
	movq 8(%rax), %rdi
	cmpq $0, %rdi
	je cond_false0
	movq -32(%rbp), %rax
cond_false0:
	movq %rax, -40(%rbp)
	movq -40(%rbp), %rax
	movq 8(%rax), %rsi
	testq %rsi, %rsi
	jz b_false1
	leaq true_string, %rdi
	call printf_wrapper
	jmp b_end1
b_false1:
	leaq false_string, %rdi
	call printf_wrapper
b_end1:
	movq $16, %rdi
	call malloc_wrapper
	movq $0, 0(%rax)
	movq $0, 8(%rax)
	movq %rax, -48(%rbp)
	movq -48(%rbp), %rax
	leaq none_string, %rdi
	call printf_wrapper
	subq $-48, %rsp
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
none_string:
	.string "None\n"
