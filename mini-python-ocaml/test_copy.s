	.text
my_malloc:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
#allign rsp to 16 bytes
	call malloc
	testq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq $8, %rdi
	call my_malloc
	movq $85, 0(%rax)
	movq %rax, -8(%rbp)
#int
	movq -8(%rbp), %rax
	movq 0(%rax), %rsi
	leaq print_int, %rdi
	call printf
	movq $8, %rdi
	call my_malloc
	movq $9, 0(%rax)
	movq %rax, -16(%rbp)
#int
	movq -16(%rbp), %rax
	movq 0(%rax), %rsi
	leaq print_int, %rdi
	call printf
#int
	movq -8(%rbp), %rax
	movq 0(%rax), %rsi
	leaq print_int, %rdi
	call printf
	addq $16, %rsp
	testq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
print_int:
	.string " %d\n"
