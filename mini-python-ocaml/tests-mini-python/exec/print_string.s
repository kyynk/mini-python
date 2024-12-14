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
	addq $0, %rsp
#print_str
	movq $8, %rdi
	call malloc_wrapper
	movabsq $str0, %rax
	movq %rax, %rsi
	leaq print_str, %rdi
	call printf_wrapper
	subq $0, %rsp
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
str0:
	.string "foo"
print_int:
	.string "%d\n"
print_str:
	.string "%s\n"
