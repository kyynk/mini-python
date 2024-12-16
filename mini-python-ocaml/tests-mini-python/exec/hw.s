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
	call printf
	movq $1, %rdi
	call exit
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	addq $0, %rsp
#print_str
	movq $120, %rdi
	call malloc_wrapper
	movq $3, 0(%rax)
	movq $12, 8(%rax)
	movq $104, 24(%rax)
	movq $101, 32(%rax)
	movq $108, 40(%rax)
	movq $108, 48(%rax)
	movq $111, 56(%rax)
	movq $44, 64(%rax)
	movq $32, 72(%rax)
	movq $119, 80(%rax)
	movq $111, 88(%rax)
	movq $114, 96(%rax)
	movq $108, 104(%rax)
	movq $100, 112(%rax)
	movq %rax, %rsi
	leaq print_str, %rdi
	call printf_wrapper
	subq $0, %rsp
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
