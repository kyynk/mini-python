	.text
malloc_wrapper:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
#allign rsp to 16 bytes
	call malloc
	movq %rbp, %rsp
	popq %rbp
	ret
printf_wrapper:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
#allign rsp to 16 bytes
	call printf
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $16, %rdi
	call malloc_wrapper
	movq $1, 0(%rax)

	movq $2, 8(%rax)
#print_int
	pushq %rax
	movq 0(%rax), %rsi
	leaq print_int, %rdi
	call printf_wrapper
	popq %rax
	movq 8(%rax), %rsi
	leaq print_int, %rdi
	call printf_wrapper
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
print_int:
	.string "%d\n"
print_str:
	.string "%s\n"
