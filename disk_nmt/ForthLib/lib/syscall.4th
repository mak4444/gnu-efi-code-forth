
CODE 1XSYS
	mov    (%rbp),%rcx

	test	$8, %rsp
        je      0f

	sub    $0x28,%rsp
	CALL	%rax
	add    $0x28,%rsp
	lea    8(%rbp),%rbp
	RET

0:	sub    $0x30,%rsp
	CALL	%rax
	add    $0x30,%rsp
	lea    8(%rbp),%rbp
	RET

END-CODE


CODE 2XSYS
	mov    8(%rbp),%rdx
	mov    (%rbp),%rcx

	test	$8, %rsp
        je      0f

	sub    $0x28,%rsp
	CALL	%rax
	add    $0x28,%rsp
	lea    0x10(%rbp),%rbp
	RET

0:	sub    $0x30,%rsp
	CALL	%rax
	add    $0x30,%rsp
	lea    0x10(%rbp),%rbp
	RET

END-CODE

CODE 3XSYS
	mov    0x10(%rbp),%r8
	mov    8(%rbp),%rdx
	mov    (%rbp),%rcx

	test	$8, %rsp
        je      0f

	sub    $0x28,%rsp
	CALL	%rax
	add    $0x28,%rsp
	lea    0x18(%rbp),%rbp
	RET

0:	sub    $0x30,%rsp
	CALL	%rax
	add    $0x30,%rsp
	lea    0x18(%rbp),%rbp
	RET
END-CODE

CODE 4XSYS
	mov    0x18(%rbp),%r9
	mov    0x10(%rbp),%r8
	mov    8(%rbp),%rdx
	mov    (%rbp),%rcx

	test	$8, %rsp
        je      0f

	sub    $0x28,%rsp
	CALL	%rax
	add    $0x28,%rsp
	lea    0x20(%rbp),%rbp
	RET

0:	sub    $0x30,%rsp
	CALL	%rax
	add    $0x30,%rsp
	lea	0x20(%rbp),%rbp
	RET
END-CODE

CODE 5XSYS
	mov    0x18(%rbp),%r9
	mov    0x10(%rbp),%r8
	mov    8(%rbp),%rdx
	mov    (%rbp),%rcx

	test	$8, %rsp
        je      0f

	sub    $0x48,%rsp
	mov    0x20(%rbp),%rbx
	mov    %rbx 0x20(%rsp)

	CALL	%rax
	add    $0x48,%rsp
	lea	0x28(%rbp),%rbp
	RET

0:	sub    $0x50,%rsp
	mov    0x20(%rbp),%rbx
	mov    %rbx 0x20(%rsp)
	CALL	%rax
	add    $0x50,%rsp
	lea	0x28(%rbp),%rbp
	RET
END-CODE


CODE 6XSYS
	mov    0x18(%rbp),%r9
	mov    0x10(%rbp),%r8
	mov    8(%rbp),%rdx
	mov    (%rbp),%rcx

	test	$8, %rsp
        je      0f

	sub    $0x48,%rsp

	mov    0x20(%rbp),%rbx
	mov    %rbx 0x20(%rsp)

	mov    0x28(%rbp),%rbx
	mov    %rbx 0x28(%rsp)

	CALL	%rax

	add    $0x48,%rsp

	lea	0x30(%rbp),%rbp
	RET

0:	sub    $0x50,%rsp

	mov    0x20(%rbp),%rbx
	mov    %rbx 0x20(%rsp)

	mov    0x28(%rbp),%rbx
	mov    %rbx 0x28(%rsp)

	CALL	%rax

	add    $0x50,%rsp

	lea	0x30(%rbp),%rbp
	RET
END-CODE

