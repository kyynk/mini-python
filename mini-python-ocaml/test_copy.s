	.text
malloc_wrapper:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
#allign rsp to 16 bytes
	call malloc
	testq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
printf_wrapper:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
#allign rsp to 16 bytes
	call printf
	testq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	movq $8, %rdi
	call malloc_wrapper
	movq $1, 0(%rax)
	movq %rax, -8(%rbp)
#print
	movq 0(%rax), %rsi
	leaq print_int, %rdi
	call printf_wrapper
	addq $8, %rsp
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
print_int:
	.string "%d\n"
print_str:
	.string "%s\n"
