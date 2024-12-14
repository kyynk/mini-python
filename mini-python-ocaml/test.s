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
	addq $-40, %rsp
	movq $8, %rdi
	call malloc_wrapper
	movq $5, 0(%rax)
	movq 0(%rax), %rdi
	pushq %rdi
	movq $8, %rdi
	call malloc_wrapper
	movq $5, 0(%rax)
	popq %rdi
	movq 0(%rax), %rsi
	cmpq %rsi, %rdi
	sete %dil
	movzbq %dil, %rdi
	pushq %rdi
	movq $8, %rdi
	call malloc_wrapper
	popq %rdi
	movq %rdi, 0(%rax)
	movq %rax, -8(%rbp)
#print_int
	movq -8(%rbp), %rax
	movq 0(%rax), %rsi
	leaq print_int, %rdi
	call printf_wrapper
	movq $8, %rdi
	call malloc_wrapper
	movq $5, 0(%rax)
	movq 0(%rax), %rdi
	pushq %rdi
	movq $8, %rdi
	call malloc_wrapper
	movq $6, 0(%rax)
	popq %rdi
	movq 0(%rax), %rsi
	cmpq %rsi, %rdi
	sete %dil
	movzbq %dil, %rdi
	pushq %rdi
	movq $8, %rdi
	call malloc_wrapper
	popq %rdi
	movq %rdi, 0(%rax)
	movq %rax, -16(%rbp)
#print_int
	movq -16(%rbp), %rax
	movq 0(%rax), %rsi
	leaq print_int, %rdi
	call printf_wrapper
	movq $8, %rdi
	call malloc_wrapper
	movabsq $str0, %rax
	movq %rax, -24(%rbp)
	movq $8, %rdi
	call malloc_wrapper
	movabsq $str1, %rax
	movq %rax, -32(%rbp)
	movq $8, %rdi
	call malloc_wrapper
	movabsq $str2, %rax
	movq %rax, -40(%rbp)
#print_str
	movq -40(%rbp), %rax
	movq %rax, %rsi
	leaq print_str, %rdi
	call printf_wrapper
#print_str
	movq -24(%rbp), %rax
	movq %rax, %rsi
	leaq print_str, %rdi
	call printf_wrapper
	subq $-40, %rsp
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
str0:
	.string "hello"
str1:
	.string "he"
str2:
	.string "llo"
print_int:
	.string "%d\n"
print_str:
	.string "%s\n"
