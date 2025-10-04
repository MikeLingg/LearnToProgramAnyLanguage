	.file	"MazeConditions.cpp"
	.text
#APP
	.globl _ZSt21ios_base_library_initv
#NO_APP
	.section	.text._ZNSt6chrono8durationIlSt5ratioILl1ELl1000000000EEEC1IlvEERKT_,"axG",@progbits,_ZNSt6chrono8durationIlSt5ratioILl1ELl1000000000EEEC1IlvEERKT_,comdat
	.align 2
	.weak	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000000000EEEC1IlvEERKT_
	.type	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000000000EEEC1IlvEERKT_, @function
_ZNSt6chrono8durationIlSt5ratioILl1ELl1000000000EEEC1IlvEERKT_:
.LFB2251:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rdx
	movq	-8(%rbp), %rax
	movq	%rdx, (%rax)
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2251:
	.size	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000000000EEEC1IlvEERKT_, .-_ZNSt6chrono8durationIlSt5ratioILl1ELl1000000000EEEC1IlvEERKT_
	.section	.text._ZNSt6chrono15duration_valuesIlE4zeroEv,"axG",@progbits,_ZNSt6chrono15duration_valuesIlE4zeroEv,comdat
	.weak	_ZNSt6chrono15duration_valuesIlE4zeroEv
	.type	_ZNSt6chrono15duration_valuesIlE4zeroEv, @function
_ZNSt6chrono15duration_valuesIlE4zeroEv:
.LFB2252:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$0, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2252:
	.size	_ZNSt6chrono15duration_valuesIlE4zeroEv, .-_ZNSt6chrono15duration_valuesIlE4zeroEv
	.section	.text._ZNKSt6chrono8durationIlSt5ratioILl1ELl1000000000EEE5countEv,"axG",@progbits,_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000000000EEE5countEv,comdat
	.align 2
	.weak	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000000000EEE5countEv
	.type	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000000000EEE5countEv, @function
_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000000000EEE5countEv:
.LFB2253:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2253:
	.size	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000000000EEE5countEv, .-_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000000000EEE5countEv
	.section	.text._ZNSt6chrono8durationIlSt5ratioILl1ELl1EEEC1IlvEERKT_,"axG",@progbits,_ZNSt6chrono8durationIlSt5ratioILl1ELl1EEEC1IlvEERKT_,comdat
	.align 2
	.weak	_ZNSt6chrono8durationIlSt5ratioILl1ELl1EEEC1IlvEERKT_
	.type	_ZNSt6chrono8durationIlSt5ratioILl1ELl1EEEC1IlvEERKT_, @function
_ZNSt6chrono8durationIlSt5ratioILl1ELl1EEEC1IlvEERKT_:
.LFB2272:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rdx
	movq	-8(%rbp), %rax
	movq	%rdx, (%rax)
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2272:
	.size	_ZNSt6chrono8durationIlSt5ratioILl1ELl1EEEC1IlvEERKT_, .-_ZNSt6chrono8durationIlSt5ratioILl1ELl1EEEC1IlvEERKT_
	.section	.rodata
.LC0:
	.string	"Initial Maze:"
.LC1:
	.string	"\nRemoving wall from cell 0:"
.LC2:
	.string	"\nRemoving wall from cell 1:"
.LC3:
	.string	"\nRemoving wall from cell 2:"
.LC4:
	.string	"\nRemoving wall from cell 3:"
.LC5:
	.string	"\nRemoving wall from cell 4:"
.LC6:
	.string	"\nRemoving wall from cell 5:"
.LC7:
	.string	"\nRemoving wall from cell 6:"
.LC8:
	.string	"\nRemoving wall from cell 7:"
.LC9:
	.string	"\nRemoving wall from cell 8:"
	.text
	.globl	main
	.type	main, @function
main:
.LFB2387:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$224, %rsp
	movq	%fs:40, %rax
	movq	%rax, -8(%rbp)
	xorl	%eax, %eax
	movl	$0, -200(%rbp)
	movl	$-1, -160(%rbp)
	movl	$-1, -156(%rbp)
	movl	$0, -152(%rbp)
	movl	$2, -148(%rbp)
	movl	$0, -144(%rbp)
	movl	$-1, -140(%rbp)
	movl	$1, -136(%rbp)
	movl	$3, -132(%rbp)
	movl	$1, -128(%rbp)
	movl	$-1, -124(%rbp)
	movl	$-1, -120(%rbp)
	movl	$4, -116(%rbp)
	movl	$-1, -112(%rbp)
	movl	$2, -108(%rbp)
	movl	$5, -104(%rbp)
	movl	$7, -100(%rbp)
	movl	$5, -96(%rbp)
	movl	$3, -92(%rbp)
	movl	$6, -88(%rbp)
	movl	$8, -84(%rbp)
	movl	$6, -80(%rbp)
	movl	$4, -76(%rbp)
	movl	$-1, -72(%rbp)
	movl	$9, -68(%rbp)
	movl	$-1, -64(%rbp)
	movl	$7, -60(%rbp)
	movl	$10, -56(%rbp)
	movl	$-1, -52(%rbp)
	movl	$10, -48(%rbp)
	movl	$8, -44(%rbp)
	movl	$11, -40(%rbp)
	movl	$-1, -36(%rbp)
	movl	$11, -32(%rbp)
	movl	$9, -28(%rbp)
	movl	$-1, -24(%rbp)
	movl	$-1, -20(%rbp)
	movb	$1, -172(%rbp)
	movb	$1, -171(%rbp)
	movb	$1, -170(%rbp)
	movb	$1, -169(%rbp)
	movb	$1, -168(%rbp)
	movb	$1, -167(%rbp)
	movb	$1, -166(%rbp)
	movb	$1, -165(%rbp)
	movb	$1, -164(%rbp)
	movb	$1, -163(%rbp)
	movb	$1, -162(%rbp)
	movb	$1, -161(%rbp)
	leaq	.LC0(%rip), %rax
	movq	%rax, %rdi
	call	puts@PLT
	movl	$0, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L8
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L9
.L8:
	movl	$32, %edi
	call	putchar@PLT
.L9:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L10
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L11
.L10:
	movl	$32, %edi
	call	putchar@PLT
.L11:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L12
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L13
.L12:
	movl	$32, %edi
	call	putchar@PLT
.L13:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L14
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L15
.L14:
	movl	$32, %edi
	call	putchar@PLT
.L15:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L16
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L17
.L16:
	movl	$32, %edi
	call	putchar@PLT
.L17:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L18
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L19
.L18:
	movl	$32, %edi
	call	putchar@PLT
.L19:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L20
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L21
.L20:
	movl	$32, %edi
	call	putchar@PLT
.L21:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L22
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L23
.L22:
	movl	$32, %edi
	call	putchar@PLT
.L23:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L24
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L25
.L24:
	movl	$32, %edi
	call	putchar@PLT
.L25:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L26
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L27
.L26:
	movl	$32, %edi
	call	putchar@PLT
.L27:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L28
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L29
.L28:
	movl	$32, %edi
	call	putchar@PLT
.L29:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L30
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L31
.L30:
	movl	$32, %edi
	call	putchar@PLT
.L31:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	leaq	.LC1(%rip), %rax
	movq	%rax, %rdi
	call	puts@PLT
	movl	$0, -192(%rbp)
	movl	$-1, -188(%rbp)
	movb	$0, -209(%rbp)
	movl	-200(%rbp), %eax
	movl	%eax, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L32
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L32
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L32:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L33
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L33
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L33
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L33:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L34
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L34
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L34
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L34:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L35
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L35
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L35
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L35:
	movl	$500, -208(%rbp)
	leaq	-208(%rbp), %rdx
	leaq	-184(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IivEERKT_
	leaq	-184(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt11this_thread9sleep_forIlSt5ratioILl1ELl1000EEEEvRKNSt6chrono8durationIT_T0_EE
	movl	$0, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L36
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L37
.L36:
	movl	$32, %edi
	call	putchar@PLT
.L37:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L38
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L39
.L38:
	movl	$32, %edi
	call	putchar@PLT
.L39:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L40
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L41
.L40:
	movl	$32, %edi
	call	putchar@PLT
.L41:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L42
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L43
.L42:
	movl	$32, %edi
	call	putchar@PLT
.L43:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L44
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L45
.L44:
	movl	$32, %edi
	call	putchar@PLT
.L45:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L46
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L47
.L46:
	movl	$32, %edi
	call	putchar@PLT
.L47:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L48
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L49
.L48:
	movl	$32, %edi
	call	putchar@PLT
.L49:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L50
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L51
.L50:
	movl	$32, %edi
	call	putchar@PLT
.L51:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L52
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L53
.L52:
	movl	$32, %edi
	call	putchar@PLT
.L53:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L54
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L55
.L54:
	movl	$32, %edi
	call	putchar@PLT
.L55:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L56
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L57
.L56:
	movl	$32, %edi
	call	putchar@PLT
.L57:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L58
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L59
.L58:
	movl	$32, %edi
	call	putchar@PLT
.L59:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	leaq	.LC2(%rip), %rax
	movq	%rax, %rdi
	call	puts@PLT
	movl	$1, -192(%rbp)
	movb	$0, -209(%rbp)
	movl	-200(%rbp), %eax
	movl	%eax, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L60
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L60
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L60:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L61
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L61
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L61
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L61:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L62
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L62
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L62
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L62:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L63
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L63
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L63
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L63:
	movl	$500, -208(%rbp)
	leaq	-208(%rbp), %rdx
	leaq	-184(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IivEERKT_
	leaq	-184(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt11this_thread9sleep_forIlSt5ratioILl1ELl1000EEEEvRKNSt6chrono8durationIT_T0_EE
	movl	$0, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L64
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L65
.L64:
	movl	$32, %edi
	call	putchar@PLT
.L65:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L66
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L67
.L66:
	movl	$32, %edi
	call	putchar@PLT
.L67:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L68
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L69
.L68:
	movl	$32, %edi
	call	putchar@PLT
.L69:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L70
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L71
.L70:
	movl	$32, %edi
	call	putchar@PLT
.L71:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L72
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L73
.L72:
	movl	$32, %edi
	call	putchar@PLT
.L73:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L74
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L75
.L74:
	movl	$32, %edi
	call	putchar@PLT
.L75:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L76
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L77
.L76:
	movl	$32, %edi
	call	putchar@PLT
.L77:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L78
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L79
.L78:
	movl	$32, %edi
	call	putchar@PLT
.L79:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L80
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L81
.L80:
	movl	$32, %edi
	call	putchar@PLT
.L81:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L82
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L83
.L82:
	movl	$32, %edi
	call	putchar@PLT
.L83:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L84
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L85
.L84:
	movl	$32, %edi
	call	putchar@PLT
.L85:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L86
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L87
.L86:
	movl	$32, %edi
	call	putchar@PLT
.L87:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	leaq	.LC3(%rip), %rax
	movq	%rax, %rdi
	call	puts@PLT
	movl	$2, -192(%rbp)
	movb	$0, -209(%rbp)
	movl	-200(%rbp), %eax
	movl	%eax, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L88
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L88
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L88:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L89
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L89
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L89
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L89:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L90
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L90
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L90
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L90:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L91
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L91
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L91
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L91:
	movl	$500, -208(%rbp)
	leaq	-208(%rbp), %rdx
	leaq	-184(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IivEERKT_
	leaq	-184(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt11this_thread9sleep_forIlSt5ratioILl1ELl1000EEEEvRKNSt6chrono8durationIT_T0_EE
	movl	$0, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L92
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L93
.L92:
	movl	$32, %edi
	call	putchar@PLT
.L93:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L94
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L95
.L94:
	movl	$32, %edi
	call	putchar@PLT
.L95:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L96
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L97
.L96:
	movl	$32, %edi
	call	putchar@PLT
.L97:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L98
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L99
.L98:
	movl	$32, %edi
	call	putchar@PLT
.L99:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L100
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L101
.L100:
	movl	$32, %edi
	call	putchar@PLT
.L101:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L102
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L103
.L102:
	movl	$32, %edi
	call	putchar@PLT
.L103:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L104
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L105
.L104:
	movl	$32, %edi
	call	putchar@PLT
.L105:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L106
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L107
.L106:
	movl	$32, %edi
	call	putchar@PLT
.L107:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L108
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L109
.L108:
	movl	$32, %edi
	call	putchar@PLT
.L109:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L110
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L111
.L110:
	movl	$32, %edi
	call	putchar@PLT
.L111:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L112
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L113
.L112:
	movl	$32, %edi
	call	putchar@PLT
.L113:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L114
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L115
.L114:
	movl	$32, %edi
	call	putchar@PLT
.L115:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	leaq	.LC4(%rip), %rax
	movq	%rax, %rdi
	call	puts@PLT
	movl	$3, -192(%rbp)
	movb	$0, -209(%rbp)
	movl	-200(%rbp), %eax
	movl	%eax, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L116
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L116
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L116:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L117
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L117
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L117
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L117:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L118
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L118
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L118
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L118:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L119
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L119
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L119
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L119:
	movl	$500, -208(%rbp)
	leaq	-208(%rbp), %rdx
	leaq	-184(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IivEERKT_
	leaq	-184(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt11this_thread9sleep_forIlSt5ratioILl1ELl1000EEEEvRKNSt6chrono8durationIT_T0_EE
	movl	$0, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L120
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L121
.L120:
	movl	$32, %edi
	call	putchar@PLT
.L121:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L122
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L123
.L122:
	movl	$32, %edi
	call	putchar@PLT
.L123:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L124
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L125
.L124:
	movl	$32, %edi
	call	putchar@PLT
.L125:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L126
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L127
.L126:
	movl	$32, %edi
	call	putchar@PLT
.L127:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L128
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L129
.L128:
	movl	$32, %edi
	call	putchar@PLT
.L129:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L130
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L131
.L130:
	movl	$32, %edi
	call	putchar@PLT
.L131:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L132
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L133
.L132:
	movl	$32, %edi
	call	putchar@PLT
.L133:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L134
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L135
.L134:
	movl	$32, %edi
	call	putchar@PLT
.L135:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L136
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L137
.L136:
	movl	$32, %edi
	call	putchar@PLT
.L137:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L138
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L139
.L138:
	movl	$32, %edi
	call	putchar@PLT
.L139:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L140
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L141
.L140:
	movl	$32, %edi
	call	putchar@PLT
.L141:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L142
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L143
.L142:
	movl	$32, %edi
	call	putchar@PLT
.L143:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	leaq	.LC5(%rip), %rax
	movq	%rax, %rdi
	call	puts@PLT
	movl	$4, -192(%rbp)
	movb	$0, -209(%rbp)
	movl	-200(%rbp), %eax
	movl	%eax, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L144
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L144
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L144:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L145
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L145
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L145
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L145:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L146
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L146
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L146
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L146:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L147
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L147
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L147
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L147:
	movl	$500, -208(%rbp)
	leaq	-208(%rbp), %rdx
	leaq	-184(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IivEERKT_
	leaq	-184(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt11this_thread9sleep_forIlSt5ratioILl1ELl1000EEEEvRKNSt6chrono8durationIT_T0_EE
	movl	$0, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L148
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L149
.L148:
	movl	$32, %edi
	call	putchar@PLT
.L149:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L150
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L151
.L150:
	movl	$32, %edi
	call	putchar@PLT
.L151:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L152
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L153
.L152:
	movl	$32, %edi
	call	putchar@PLT
.L153:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L154
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L155
.L154:
	movl	$32, %edi
	call	putchar@PLT
.L155:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L156
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L157
.L156:
	movl	$32, %edi
	call	putchar@PLT
.L157:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L158
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L159
.L158:
	movl	$32, %edi
	call	putchar@PLT
.L159:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L160
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L161
.L160:
	movl	$32, %edi
	call	putchar@PLT
.L161:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L162
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L163
.L162:
	movl	$32, %edi
	call	putchar@PLT
.L163:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L164
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L165
.L164:
	movl	$32, %edi
	call	putchar@PLT
.L165:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L166
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L167
.L166:
	movl	$32, %edi
	call	putchar@PLT
.L167:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L168
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L169
.L168:
	movl	$32, %edi
	call	putchar@PLT
.L169:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L170
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L171
.L170:
	movl	$32, %edi
	call	putchar@PLT
.L171:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	leaq	.LC6(%rip), %rax
	movq	%rax, %rdi
	call	puts@PLT
	movl	$5, -192(%rbp)
	movb	$0, -209(%rbp)
	movl	-200(%rbp), %eax
	movl	%eax, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L172
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L172
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L172:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L173
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L173
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L173
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L173:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L174
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L174
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L174
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L174:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L175
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L175
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L175
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L175:
	movl	$500, -208(%rbp)
	leaq	-208(%rbp), %rdx
	leaq	-184(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IivEERKT_
	leaq	-184(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt11this_thread9sleep_forIlSt5ratioILl1ELl1000EEEEvRKNSt6chrono8durationIT_T0_EE
	movl	$0, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L176
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L177
.L176:
	movl	$32, %edi
	call	putchar@PLT
.L177:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L178
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L179
.L178:
	movl	$32, %edi
	call	putchar@PLT
.L179:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L180
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L181
.L180:
	movl	$32, %edi
	call	putchar@PLT
.L181:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L182
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L183
.L182:
	movl	$32, %edi
	call	putchar@PLT
.L183:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L184
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L185
.L184:
	movl	$32, %edi
	call	putchar@PLT
.L185:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L186
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L187
.L186:
	movl	$32, %edi
	call	putchar@PLT
.L187:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L188
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L189
.L188:
	movl	$32, %edi
	call	putchar@PLT
.L189:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L190
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L191
.L190:
	movl	$32, %edi
	call	putchar@PLT
.L191:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L192
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L193
.L192:
	movl	$32, %edi
	call	putchar@PLT
.L193:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L194
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L195
.L194:
	movl	$32, %edi
	call	putchar@PLT
.L195:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L196
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L197
.L196:
	movl	$32, %edi
	call	putchar@PLT
.L197:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L198
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L199
.L198:
	movl	$32, %edi
	call	putchar@PLT
.L199:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	leaq	.LC7(%rip), %rax
	movq	%rax, %rdi
	call	puts@PLT
	movl	$6, -192(%rbp)
	movb	$0, -209(%rbp)
	movl	-200(%rbp), %eax
	movl	%eax, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L200
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L200
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L200:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L201
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L201
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L201
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L201:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L202
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L202
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L202
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L202:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L203
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L203
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L203
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L203:
	movl	$500, -208(%rbp)
	leaq	-208(%rbp), %rdx
	leaq	-184(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IivEERKT_
	leaq	-184(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt11this_thread9sleep_forIlSt5ratioILl1ELl1000EEEEvRKNSt6chrono8durationIT_T0_EE
	movl	$0, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L204
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L205
.L204:
	movl	$32, %edi
	call	putchar@PLT
.L205:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L206
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L207
.L206:
	movl	$32, %edi
	call	putchar@PLT
.L207:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L208
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L209
.L208:
	movl	$32, %edi
	call	putchar@PLT
.L209:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L210
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L211
.L210:
	movl	$32, %edi
	call	putchar@PLT
.L211:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L212
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L213
.L212:
	movl	$32, %edi
	call	putchar@PLT
.L213:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L214
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L215
.L214:
	movl	$32, %edi
	call	putchar@PLT
.L215:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L216
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L217
.L216:
	movl	$32, %edi
	call	putchar@PLT
.L217:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L218
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L219
.L218:
	movl	$32, %edi
	call	putchar@PLT
.L219:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L220
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L221
.L220:
	movl	$32, %edi
	call	putchar@PLT
.L221:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L222
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L223
.L222:
	movl	$32, %edi
	call	putchar@PLT
.L223:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L224
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L225
.L224:
	movl	$32, %edi
	call	putchar@PLT
.L225:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L226
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L227
.L226:
	movl	$32, %edi
	call	putchar@PLT
.L227:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	leaq	.LC8(%rip), %rax
	movq	%rax, %rdi
	call	puts@PLT
	movl	$7, -192(%rbp)
	movb	$0, -209(%rbp)
	movl	-200(%rbp), %eax
	movl	%eax, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L228
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L228
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L228:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L229
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L229
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L229
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L229:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L230
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L230
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L230
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L230:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L231
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L231
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L231
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L231:
	movl	$500, -208(%rbp)
	leaq	-208(%rbp), %rdx
	leaq	-184(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IivEERKT_
	leaq	-184(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt11this_thread9sleep_forIlSt5ratioILl1ELl1000EEEEvRKNSt6chrono8durationIT_T0_EE
	movl	$0, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L232
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L233
.L232:
	movl	$32, %edi
	call	putchar@PLT
.L233:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L234
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L235
.L234:
	movl	$32, %edi
	call	putchar@PLT
.L235:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L236
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L237
.L236:
	movl	$32, %edi
	call	putchar@PLT
.L237:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L238
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L239
.L238:
	movl	$32, %edi
	call	putchar@PLT
.L239:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L240
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L241
.L240:
	movl	$32, %edi
	call	putchar@PLT
.L241:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L242
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L243
.L242:
	movl	$32, %edi
	call	putchar@PLT
.L243:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L244
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L245
.L244:
	movl	$32, %edi
	call	putchar@PLT
.L245:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L246
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L247
.L246:
	movl	$32, %edi
	call	putchar@PLT
.L247:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L248
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L249
.L248:
	movl	$32, %edi
	call	putchar@PLT
.L249:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L250
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L251
.L250:
	movl	$32, %edi
	call	putchar@PLT
.L251:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L252
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L253
.L252:
	movl	$32, %edi
	call	putchar@PLT
.L253:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L254
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L255
.L254:
	movl	$32, %edi
	call	putchar@PLT
.L255:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	leaq	.LC9(%rip), %rax
	movq	%rax, %rdi
	call	puts@PLT
	movl	$8, -192(%rbp)
	movb	$0, -209(%rbp)
	movl	-200(%rbp), %eax
	movl	%eax, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L256
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L256
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L256:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L257
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L257
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L257
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L257:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L258
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L258
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L258
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L258:
	movzbl	-209(%rbp), %eax
	testl	%eax, %eax
	jne	.L259
	addl	$1, -204(%rbp)
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	testl	%eax, %eax
	js	.L259
	movl	-204(%rbp), %eax
	cltq
	movl	-192(%rbp), %edx
	movslq	%edx, %rdx
	salq	$2, %rdx
	addq	%rdx, %rax
	movl	-160(%rbp,%rax,4), %eax
	movl	%eax, -188(%rbp)
	movl	-188(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L259
	movl	-188(%rbp), %eax
	cltq
	movb	$0, -172(%rbp,%rax)
	movb	$1, -209(%rbp)
.L259:
	movl	$500, -208(%rbp)
	leaq	-208(%rbp), %rdx
	leaq	-184(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IivEERKT_
	leaq	-184(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt11this_thread9sleep_forIlSt5ratioILl1ELl1000EEEEvRKNSt6chrono8durationIT_T0_EE
	movl	$0, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L260
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L261
.L260:
	movl	$32, %edi
	call	putchar@PLT
.L261:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L262
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L263
.L262:
	movl	$32, %edi
	call	putchar@PLT
.L263:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L264
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L265
.L264:
	movl	$32, %edi
	call	putchar@PLT
.L265:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L266
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L267
.L266:
	movl	$32, %edi
	call	putchar@PLT
.L267:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L268
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L269
.L268:
	movl	$32, %edi
	call	putchar@PLT
.L269:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L270
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L271
.L270:
	movl	$32, %edi
	call	putchar@PLT
.L271:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L272
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L273
.L272:
	movl	$32, %edi
	call	putchar@PLT
.L273:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L274
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L275
.L274:
	movl	$32, %edi
	call	putchar@PLT
.L275:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L276
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L277
.L276:
	movl	$32, %edi
	call	putchar@PLT
.L277:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L278
	movl	$45, %edi
	call	putchar@PLT
	jmp	.L279
.L278:
	movl	$32, %edi
	call	putchar@PLT
.L279:
	addl	$1, -196(%rbp)
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L280
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L281
.L280:
	movl	$32, %edi
	call	putchar@PLT
.L281:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	-196(%rbp), %eax
	cltq
	movzbl	-172(%rbp,%rax), %eax
	movzbl	%al, %eax
	cmpl	$1, %eax
	jne	.L282
	movl	$124, %edi
	call	putchar@PLT
	jmp	.L283
.L282:
	movl	$32, %edi
	call	putchar@PLT
.L283:
	addl	$1, -196(%rbp)
	movl	$32, %edi
	call	putchar@PLT
	movl	$124, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$45, %edi
	call	putchar@PLT
	movl	$43, %edi
	call	putchar@PLT
	movl	$10, %edi
	call	putchar@PLT
	movl	$0, %eax
	movq	-8(%rbp), %rdx
	subq	%fs:40, %rdx
	je	.L285
	call	__stack_chk_fail@PLT
.L285:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2387:
	.size	main, .-main
	.section	.text._ZNKSt6chrono8durationIlSt5ratioILl1ELl1EEE5countEv,"axG",@progbits,_ZNKSt6chrono8durationIlSt5ratioILl1ELl1EEE5countEv,comdat
	.align 2
	.weak	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1EEE5countEv
	.type	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1EEE5countEv, @function
_ZNKSt6chrono8durationIlSt5ratioILl1ELl1EEE5countEv:
.LFB2645:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2645:
	.size	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1EEE5countEv, .-_ZNKSt6chrono8durationIlSt5ratioILl1ELl1EEE5countEv
	.section	.text._ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IivEERKT_,"axG",@progbits,_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC5IivEERKT_,comdat
	.align 2
	.weak	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IivEERKT_
	.type	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IivEERKT_, @function
_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IivEERKT_:
.LFB2673:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movl	(%rax), %eax
	movslq	%eax, %rdx
	movq	-8(%rbp), %rax
	movq	%rdx, (%rax)
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2673:
	.size	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IivEERKT_, .-_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IivEERKT_
	.weak	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IivEERKT_
	.set	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IivEERKT_,_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IivEERKT_
	.section	.text._ZNSt11this_thread9sleep_forIlSt5ratioILl1ELl1000EEEEvRKNSt6chrono8durationIT_T0_EE,"axG",@progbits,_ZNSt11this_thread9sleep_forIlSt5ratioILl1ELl1000EEEEvRKNSt6chrono8durationIT_T0_EE,comdat
	.weak	_ZNSt11this_thread9sleep_forIlSt5ratioILl1ELl1000EEEEvRKNSt6chrono8durationIT_T0_EE
	.type	_ZNSt11this_thread9sleep_forIlSt5ratioILl1ELl1000EEEEvRKNSt6chrono8durationIT_T0_EE, @function
_ZNSt11this_thread9sleep_forIlSt5ratioILl1ELl1000EEEEvRKNSt6chrono8durationIT_T0_EE:
.LFB2675:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$64, %rsp
	movq	%rdi, -56(%rbp)
	movq	%fs:40, %rax
	movq	%rax, -8(%rbp)
	xorl	%eax, %eax
	call	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEE4zeroEv
	movq	%rax, -32(%rbp)
	leaq	-32(%rbp), %rdx
	movq	-56(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chronoleIlSt5ratioILl1ELl1000EElS2_EEbRKNS_8durationIT_T0_EERKNS3_IT1_T2_EE
	testb	%al, %al
	jne	.L296
	movq	-56(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1EEEElS2_ILl1ELl1000EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE
	movq	%rax, -48(%rbp)
	leaq	-48(%rbp), %rdx
	movq	-56(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chronomiIlSt5ratioILl1ELl1000EElS1_ILl1ELl1EEEENSt11common_typeIJNS_8durationIT_T0_EENS5_IT1_T2_EEEE4typeERKS8_RKSB_
	movq	%rax, -32(%rbp)
	leaq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1000000000EEEElS2_ILl1ELl1000EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE
	movq	%rax, -40(%rbp)
	leaq	-48(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1EEE5countEv
	movq	%rax, -32(%rbp)
	leaq	-40(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000000000EEE5countEv
	movq	%rax, -24(%rbp)
	nop
.L294:
	leaq	-32(%rbp), %rdx
	leaq	-32(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	nanosleep@PLT
	cmpl	$-1, %eax
	jne	.L292
	call	__errno_location@PLT
	movl	(%rax), %eax
	cmpl	$4, %eax
	jne	.L292
	movl	$1, %eax
	jmp	.L293
.L292:
	movl	$0, %eax
.L293:
	testb	%al, %al
	jne	.L294
	jmp	.L289
.L296:
	nop
.L289:
	movq	-8(%rbp), %rax
	subq	%fs:40, %rax
	je	.L295
	call	__stack_chk_fail@PLT
.L295:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2675:
	.size	_ZNSt11this_thread9sleep_forIlSt5ratioILl1ELl1000EEEEvRKNSt6chrono8durationIT_T0_EE, .-_ZNSt11this_thread9sleep_forIlSt5ratioILl1ELl1000EEEEvRKNSt6chrono8durationIT_T0_EE
	.section	.text._ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEE4zeroEv,"axG",@progbits,_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEE4zeroEv,comdat
	.weak	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEE4zeroEv
	.type	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEE4zeroEv, @function
_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEE4zeroEv:
.LFB2787:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movq	%fs:40, %rax
	movq	%rax, -8(%rbp)
	xorl	%eax, %eax
	call	_ZNSt6chrono15duration_valuesIlE4zeroEv
	movq	%rax, -24(%rbp)
	leaq	-24(%rbp), %rdx
	leaq	-16(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IlvEERKT_
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	subq	%fs:40, %rdx
	je	.L299
	call	__stack_chk_fail@PLT
.L299:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2787:
	.size	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEE4zeroEv, .-_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEE4zeroEv
	.section	.text._ZNSt6chronoleIlSt5ratioILl1ELl1000EElS2_EEbRKNS_8durationIT_T0_EERKNS3_IT1_T2_EE,"axG",@progbits,_ZNSt6chronoleIlSt5ratioILl1ELl1000EElS2_EEbRKNS_8durationIT_T0_EERKNS3_IT1_T2_EE,comdat
	.weak	_ZNSt6chronoleIlSt5ratioILl1ELl1000EElS2_EEbRKNS_8durationIT_T0_EERKNS3_IT1_T2_EE
	.type	_ZNSt6chronoleIlSt5ratioILl1ELl1000EElS2_EEbRKNS_8durationIT_T0_EERKNS3_IT1_T2_EE, @function
_ZNSt6chronoleIlSt5ratioILl1ELl1000EElS2_EEbRKNS_8durationIT_T0_EERKNS3_IT1_T2_EE:
.LFB2788:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chronoltIlSt5ratioILl1ELl1000EElS2_EEbRKNS_8durationIT_T0_EERKNS3_IT1_T2_EE
	xorl	$1, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2788:
	.size	_ZNSt6chronoleIlSt5ratioILl1ELl1000EElS2_EEbRKNS_8durationIT_T0_EERKNS3_IT1_T2_EE, .-_ZNSt6chronoleIlSt5ratioILl1ELl1000EElS2_EEbRKNS_8durationIT_T0_EERKNS3_IT1_T2_EE
	.section	.text._ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1EEEElS2_ILl1ELl1000EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE,"axG",@progbits,_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1EEEElS2_ILl1ELl1000EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE,comdat
	.weak	_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1EEEElS2_ILl1ELl1000EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE
	.type	_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1EEEElS2_ILl1ELl1000EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE, @function
_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1EEEElS2_ILl1ELl1000EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE:
.LFB2789:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1EEEES2_ILl1ELl1000EElLb1ELb0EE6__castIlS5_EES4_RKNS1_IT_T0_EE
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2789:
	.size	_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1EEEElS2_ILl1ELl1000EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE, .-_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1EEEElS2_ILl1ELl1000EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE
	.section	.text._ZNSt6chronomiIlSt5ratioILl1ELl1000EElS1_ILl1ELl1EEEENSt11common_typeIJNS_8durationIT_T0_EENS5_IT1_T2_EEEE4typeERKS8_RKSB_,"axG",@progbits,_ZNSt6chronomiIlSt5ratioILl1ELl1000EElS1_ILl1ELl1EEEENSt11common_typeIJNS_8durationIT_T0_EENS5_IT1_T2_EEEE4typeERKS8_RKSB_,comdat
	.weak	_ZNSt6chronomiIlSt5ratioILl1ELl1000EElS1_ILl1ELl1EEEENSt11common_typeIJNS_8durationIT_T0_EENS5_IT1_T2_EEEE4typeERKS8_RKSB_
	.type	_ZNSt6chronomiIlSt5ratioILl1ELl1000EElS1_ILl1ELl1EEEENSt11common_typeIJNS_8durationIT_T0_EENS5_IT1_T2_EEEE4typeERKS8_RKSB_, @function
_ZNSt6chronomiIlSt5ratioILl1ELl1000EElS1_ILl1ELl1EEEENSt11common_typeIJNS_8durationIT_T0_EENS5_IT1_T2_EEEE4typeERKS8_RKSB_:
.LFB2791:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$72, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -72(%rbp)
	movq	%rsi, -80(%rbp)
	movq	%fs:40, %rax
	movq	%rax, -24(%rbp)
	xorl	%eax, %eax
	movq	-72(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -56(%rbp)
	leaq	-56(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000EEE5countEv
	movq	%rax, %rbx
	movq	-80(%rbp), %rdx
	leaq	-48(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IlS1_ILl1ELl1EEvEERKNS0_IT_T0_EE
	leaq	-48(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000EEE5countEv
	subq	%rax, %rbx
	movq	%rbx, %rdx
	movq	%rdx, -40(%rbp)
	leaq	-40(%rbp), %rdx
	leaq	-32(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IlvEERKT_
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rdx
	subq	%fs:40, %rdx
	je	.L306
	call	__stack_chk_fail@PLT
.L306:
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2791:
	.size	_ZNSt6chronomiIlSt5ratioILl1ELl1000EElS1_ILl1ELl1EEEENSt11common_typeIJNS_8durationIT_T0_EENS5_IT1_T2_EEEE4typeERKS8_RKSB_, .-_ZNSt6chronomiIlSt5ratioILl1ELl1000EElS1_ILl1ELl1EEEENSt11common_typeIJNS_8durationIT_T0_EENS5_IT1_T2_EEEE4typeERKS8_RKSB_
	.section	.text._ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1000000000EEEElS2_ILl1ELl1000EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE,"axG",@progbits,_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1000000000EEEElS2_ILl1ELl1000EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE,comdat
	.weak	_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1000000000EEEElS2_ILl1ELl1000EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE
	.type	_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1000000000EEEElS2_ILl1ELl1000EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE, @function
_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1000000000EEEElS2_ILl1ELl1000EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE:
.LFB2793:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1000000000EEEES2_ILl1000000ELl1EElLb0ELb1EE6__castIlS2_ILl1ELl1000EEEES4_RKNS1_IT_T0_EE
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2793:
	.size	_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1000000000EEEElS2_ILl1ELl1000EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE, .-_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1000000000EEEElS2_ILl1ELl1000EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE
	.section	.text._ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IlvEERKT_,"axG",@progbits,_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC5IlvEERKT_,comdat
	.align 2
	.weak	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IlvEERKT_
	.type	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IlvEERKT_, @function
_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IlvEERKT_:
.LFB2864:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rdx
	movq	-8(%rbp), %rax
	movq	%rdx, (%rax)
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2864:
	.size	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IlvEERKT_, .-_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IlvEERKT_
	.weak	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IlvEERKT_
	.set	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IlvEERKT_,_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IlvEERKT_
	.section	.text._ZNSt6chronoltIlSt5ratioILl1ELl1000EElS2_EEbRKNS_8durationIT_T0_EERKNS3_IT1_T2_EE,"axG",@progbits,_ZNSt6chronoltIlSt5ratioILl1ELl1000EElS2_EEbRKNS_8durationIT_T0_EERKNS3_IT1_T2_EE,comdat
	.weak	_ZNSt6chronoltIlSt5ratioILl1ELl1000EElS2_EEbRKNS_8durationIT_T0_EERKNS3_IT1_T2_EE
	.type	_ZNSt6chronoltIlSt5ratioILl1ELl1000EElS2_EEbRKNS_8durationIT_T0_EERKNS3_IT1_T2_EE, @function
_ZNSt6chronoltIlSt5ratioILl1ELl1000EElS2_EEbRKNS_8durationIT_T0_EERKNS3_IT1_T2_EE:
.LFB2866:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$56, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -56(%rbp)
	movq	%rsi, -64(%rbp)
	movq	%fs:40, %rax
	movq	%rax, -24(%rbp)
	xorl	%eax, %eax
	movq	-56(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -40(%rbp)
	leaq	-40(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000EEE5countEv
	movq	%rax, %rbx
	movq	-64(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -32(%rbp)
	leaq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000EEE5countEv
	cmpq	%rax, %rbx
	setl	%al
	movq	-24(%rbp), %rdx
	subq	%fs:40, %rdx
	je	.L312
	call	__stack_chk_fail@PLT
.L312:
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2866:
	.size	_ZNSt6chronoltIlSt5ratioILl1ELl1000EElS2_EEbRKNS_8durationIT_T0_EERKNS3_IT1_T2_EE, .-_ZNSt6chronoltIlSt5ratioILl1ELl1000EElS2_EEbRKNS_8durationIT_T0_EERKNS3_IT1_T2_EE
	.section	.text._ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1EEEES2_ILl1ELl1000EElLb1ELb0EE6__castIlS5_EES4_RKNS1_IT_T0_EE,"axG",@progbits,_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1EEEES2_ILl1ELl1000EElLb1ELb0EE6__castIlS5_EES4_RKNS1_IT_T0_EE,comdat
	.weak	_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1EEEES2_ILl1ELl1000EElLb1ELb0EE6__castIlS5_EES4_RKNS1_IT_T0_EE
	.type	_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1EEEES2_ILl1ELl1000EElLb1ELb0EE6__castIlS5_EES4_RKNS1_IT_T0_EE, @function
_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1EEEES2_ILl1ELl1000EElLb1ELb0EE6__castIlS5_EES4_RKNS1_IT_T0_EE:
.LFB2867:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$48, %rsp
	movq	%rdi, -40(%rbp)
	movq	%fs:40, %rax
	movq	%rax, -8(%rbp)
	xorl	%eax, %eax
	movq	-40(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000EEE5countEv
	movq	%rax, %rcx
	movabsq	$2361183241434822607, %rdx
	movq	%rcx, %rax
	imulq	%rdx
	movq	%rdx, %rax
	sarq	$7, %rax
	sarq	$63, %rcx
	movq	%rcx, %rdx
	subq	%rdx, %rax
	movq	%rax, -24(%rbp)
	leaq	-24(%rbp), %rdx
	leaq	-16(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chrono8durationIlSt5ratioILl1ELl1EEEC1IlvEERKT_
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	subq	%fs:40, %rdx
	je	.L315
	call	__stack_chk_fail@PLT
.L315:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2867:
	.size	_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1EEEES2_ILl1ELl1000EElLb1ELb0EE6__castIlS5_EES4_RKNS1_IT_T0_EE, .-_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1EEEES2_ILl1ELl1000EElLb1ELb0EE6__castIlS5_EES4_RKNS1_IT_T0_EE
	.section	.text._ZNKSt6chrono8durationIlSt5ratioILl1ELl1000EEE5countEv,"axG",@progbits,_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000EEE5countEv,comdat
	.align 2
	.weak	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000EEE5countEv
	.type	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000EEE5countEv, @function
_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000EEE5countEv:
.LFB2868:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2868:
	.size	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000EEE5countEv, .-_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000EEE5countEv
	.section	.text._ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IlS1_ILl1ELl1EEvEERKNS0_IT_T0_EE,"axG",@progbits,_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC5IlS1_ILl1ELl1EEvEERKNS0_IT_T0_EE,comdat
	.align 2
	.weak	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IlS1_ILl1ELl1EEvEERKNS0_IT_T0_EE
	.type	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IlS1_ILl1ELl1EEvEERKNS0_IT_T0_EE, @function
_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IlS1_ILl1ELl1EEvEERKNS0_IT_T0_EE:
.LFB2870:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%fs:40, %rax
	movq	%rax, -8(%rbp)
	xorl	%eax, %eax
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1000EEEElS2_ILl1ELl1EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE
	movq	%rax, -16(%rbp)
	leaq	-16(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000EEE5countEv
	movq	-24(%rbp), %rdx
	movq	%rax, (%rdx)
	nop
	movq	-8(%rbp), %rax
	subq	%fs:40, %rax
	je	.L319
	call	__stack_chk_fail@PLT
.L319:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2870:
	.size	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IlS1_ILl1ELl1EEvEERKNS0_IT_T0_EE, .-_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IlS1_ILl1ELl1EEvEERKNS0_IT_T0_EE
	.weak	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IlS1_ILl1ELl1EEvEERKNS0_IT_T0_EE
	.set	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IlS1_ILl1ELl1EEvEERKNS0_IT_T0_EE,_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC2IlS1_ILl1ELl1EEvEERKNS0_IT_T0_EE
	.section	.text._ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1000000000EEEES2_ILl1000000ELl1EElLb0ELb1EE6__castIlS2_ILl1ELl1000EEEES4_RKNS1_IT_T0_EE,"axG",@progbits,_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1000000000EEEES2_ILl1000000ELl1EElLb0ELb1EE6__castIlS2_ILl1ELl1000EEEES4_RKNS1_IT_T0_EE,comdat
	.weak	_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1000000000EEEES2_ILl1000000ELl1EElLb0ELb1EE6__castIlS2_ILl1ELl1000EEEES4_RKNS1_IT_T0_EE
	.type	_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1000000000EEEES2_ILl1000000ELl1EElLb0ELb1EE6__castIlS2_ILl1ELl1000EEEES4_RKNS1_IT_T0_EE, @function
_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1000000000EEEES2_ILl1000000ELl1EElLb0ELb1EE6__castIlS2_ILl1ELl1000EEEES4_RKNS1_IT_T0_EE:
.LFB2872:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$48, %rsp
	movq	%rdi, -40(%rbp)
	movq	%fs:40, %rax
	movq	%rax, -8(%rbp)
	xorl	%eax, %eax
	movq	-40(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1000EEE5countEv
	imulq	$1000000, %rax, %rax
	movq	%rax, -24(%rbp)
	leaq	-24(%rbp), %rdx
	leaq	-16(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000000000EEEC1IlvEERKT_
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	subq	%fs:40, %rdx
	je	.L322
	call	__stack_chk_fail@PLT
.L322:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2872:
	.size	_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1000000000EEEES2_ILl1000000ELl1EElLb0ELb1EE6__castIlS2_ILl1ELl1000EEEES4_RKNS1_IT_T0_EE, .-_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1000000000EEEES2_ILl1000000ELl1EElLb0ELb1EE6__castIlS2_ILl1ELl1000EEEES4_RKNS1_IT_T0_EE
	.section	.text._ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1000EEEElS2_ILl1ELl1EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE,"axG",@progbits,_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1000EEEElS2_ILl1ELl1EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE,comdat
	.weak	_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1000EEEElS2_ILl1ELl1EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE
	.type	_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1000EEEElS2_ILl1ELl1EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE, @function
_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1000EEEElS2_ILl1ELl1EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE:
.LFB2928:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1000EEEES2_ILl1000ELl1EElLb0ELb1EE6__castIlS2_ILl1ELl1EEEES4_RKNS1_IT_T0_EE
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2928:
	.size	_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1000EEEElS2_ILl1ELl1EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE, .-_ZNSt6chrono13duration_castINS_8durationIlSt5ratioILl1ELl1000EEEElS2_ILl1ELl1EEEENSt9enable_ifIXsrNS_13__is_durationIT_EE5valueES8_E4typeERKNS1_IT0_T1_EE
	.section	.text._ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1000EEEES2_ILl1000ELl1EElLb0ELb1EE6__castIlS2_ILl1ELl1EEEES4_RKNS1_IT_T0_EE,"axG",@progbits,_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1000EEEES2_ILl1000ELl1EElLb0ELb1EE6__castIlS2_ILl1ELl1EEEES4_RKNS1_IT_T0_EE,comdat
	.weak	_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1000EEEES2_ILl1000ELl1EElLb0ELb1EE6__castIlS2_ILl1ELl1EEEES4_RKNS1_IT_T0_EE
	.type	_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1000EEEES2_ILl1000ELl1EElLb0ELb1EE6__castIlS2_ILl1ELl1EEEES4_RKNS1_IT_T0_EE, @function
_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1000EEEES2_ILl1000ELl1EElLb0ELb1EE6__castIlS2_ILl1ELl1EEEES4_RKNS1_IT_T0_EE:
.LFB2950:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$48, %rsp
	movq	%rdi, -40(%rbp)
	movq	%fs:40, %rax
	movq	%rax, -8(%rbp)
	xorl	%eax, %eax
	movq	-40(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt6chrono8durationIlSt5ratioILl1ELl1EEE5countEv
	imulq	$1000, %rax, %rax
	movq	%rax, -24(%rbp)
	leaq	-24(%rbp), %rdx
	leaq	-16(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt6chrono8durationIlSt5ratioILl1ELl1000EEEC1IlvEERKT_
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	subq	%fs:40, %rdx
	je	.L327
	call	__stack_chk_fail@PLT
.L327:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2950:
	.size	_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1000EEEES2_ILl1000ELl1EElLb0ELb1EE6__castIlS2_ILl1ELl1EEEES4_RKNS1_IT_T0_EE, .-_ZNSt6chrono20__duration_cast_implINS_8durationIlSt5ratioILl1ELl1000EEEES2_ILl1000ELl1EElLb0ELb1EE6__castIlS2_ILl1ELl1EEEES4_RKNS1_IT_T0_EE
	.section	.rodata
	.type	_ZNSt8__detail30__integer_to_chars_is_unsignedIjEE, @object
	.size	_ZNSt8__detail30__integer_to_chars_is_unsignedIjEE, 1
_ZNSt8__detail30__integer_to_chars_is_unsignedIjEE:
	.byte	1
	.type	_ZNSt8__detail30__integer_to_chars_is_unsignedImEE, @object
	.size	_ZNSt8__detail30__integer_to_chars_is_unsignedImEE, 1
_ZNSt8__detail30__integer_to_chars_is_unsignedImEE:
	.byte	1
	.type	_ZNSt8__detail30__integer_to_chars_is_unsignedIyEE, @object
	.size	_ZNSt8__detail30__integer_to_chars_is_unsignedIyEE, 1
_ZNSt8__detail30__integer_to_chars_is_unsignedIyEE:
	.byte	1
	.type	_ZSt12__is_ratio_vISt5ratioILl1ELl1000000000EEE, @object
	.size	_ZSt12__is_ratio_vISt5ratioILl1ELl1000000000EEE, 1
_ZSt12__is_ratio_vISt5ratioILl1ELl1000000000EEE:
	.byte	1
	.type	_ZSt12__is_ratio_vISt5ratioILl1ELl1EEE, @object
	.size	_ZSt12__is_ratio_vISt5ratioILl1ELl1EEE, 1
_ZSt12__is_ratio_vISt5ratioILl1ELl1EEE:
	.byte	1
	.type	_ZSt12__is_ratio_vISt5ratioILl1000000000ELl1EEE, @object
	.size	_ZSt12__is_ratio_vISt5ratioILl1000000000ELl1EEE, 1
_ZSt12__is_ratio_vISt5ratioILl1000000000ELl1EEE:
	.byte	1
	.type	_ZSt12__is_ratio_vISt5ratioILl1ELl1000EEE, @object
	.size	_ZSt12__is_ratio_vISt5ratioILl1ELl1000EEE, 1
_ZSt12__is_ratio_vISt5ratioILl1ELl1000EEE:
	.byte	1
	.type	_ZSt12__is_ratio_vISt5ratioILl1000ELl1EEE, @object
	.size	_ZSt12__is_ratio_vISt5ratioILl1000ELl1EEE, 1
_ZSt12__is_ratio_vISt5ratioILl1000ELl1EEE:
	.byte	1
	.ident	"GCC: (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0"
	.section	.note.GNU-stack,"",@progbits
	.section	.note.gnu.property,"a"
	.align 8
	.long	1f - 0f
	.long	4f - 1f
	.long	5
0:
	.string	"GNU"
1:
	.align 8
	.long	0xc0000002
	.long	3f - 2f
2:
	.long	0x3
3:
	.align 8
4:
