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
	subq $16, %rsp
#print
#Badd
	movq $8, %rdi
	call malloc_wrapper
	movq $1, 0(%rax)
	movq %rax, -8(%rbp)
	movq 0(%rax), %rdi
#push for first value
	pushq %rdi
	movq $8, %rdi
	call malloc_wrapper
	movq $2, 0(%rax)
	movq %rax, -16(%rbp)
	popq %rdi
	movq 0(%rax), %rsi
	addq %rsi, %rdi
#push for result
	pushq %rdi
	movq $8, %rdi
	call malloc_wrapper
	popq %rdi
	movq %rdi, 0(%rax)
#Badd end
	movq 0(%rax), %rsi
	leaq print_int, %rdi
	call printf_wrapper
	addq $16, %rsp
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
print_int:
	.string "%d\n"
