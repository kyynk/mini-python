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
	addq $-8, %rsp
	movq $16, %rdi
	call malloc_wrapper
	movq $2, 0(%rax)
	movq $42, 8(%rax)
	movq %rax, -8(%rbp)
#print_int
	movq -8(%rbp), %rax
	movq 0(%rax), %rsi
	leaq print_int, %rdi
	call printf_wrapper
	subq $-8, %rsp
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
runtime_error_msg:
	.string "Runtime error occurred\n"
print_int:
	.string "%d\n"
print_str:
	.string "%s\n"
