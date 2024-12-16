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
	addq $-24, %rsp
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
	movq -8(%rbp), %rax
	movq %rax, %rsi
loop_start:
	movq 0(%rsi), %rax
	testq %rax, %rax
	jz loop_end
	movq %rax, %rdi
	pushq %rsi
	call putchar_wrapper
	popq %rsi
	addq $8, %rsi
	jmp loop_start
loop_end:
	movq $10, %rdi
	call putchar_wrapper
	movq -16(%rbp), %rax
	movq 8(%rax), %rdi
	leaq print_int, %rdi
	call printf_wrapper
	movq -24(%rbp), %rax
	movq 0(%rax), %rsi
	testq %rsi, %rsi
	jz print_false
print_true:
	leaq true_string, %rdi
	call printf_wrapper
	jmp print_end
print_false:
	leaq false_string, %rdi
	call printf_wrapper
print_end:
	subq $-24, %rsp
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
	.string "True\n"
false_string:
	.string "False\n"
