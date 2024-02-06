
HEADER XSYS1
HEADER XSYS2
HEADER XSYS3
HEADER XSYS4
HEADER XSYS5
HEADER XSYS6
HEADER XSYS7
HEADER XSYS8

CODE XSYS9
	mov    0x20(%rbp),%r14
LABSET XSYS8
	mov    0x20(%rbp),%r13
LABSET XSYS7
	mov    0x20(%rbp),%r12
LABSET XSYS6
	mov    0x20(%rbp),%r11
LABSET XSYS5
	mov    0x20(%rbp),%rcx
	mov    %rcx,0x20(%rsp)
LABSET XSYS4
	mov    0x18(%rbp),%r9
LABSET XSYS3
	mov    0x10(%rbp),%r8
LABSET XSYS2
	mov    8(%rbp),%rdx
LABSET XSYS1
	mov    (%rbp),%rcx
	CALL	%rax
	RET
END-CODE


CODE 1XSYS
	mov    (%rbp),%rcx
	CALL	%rax
	lea    8(%rbp),%rbp
	RET
END-CODE


CODE 2XSYS
	mov    8(%rbp),%rdx
	mov    (%rbp),%rcx
	CALL	%rax
	lea    0x10(%rbp),%rbp
	RET
END-CODE

CODE 3XSYS
	CALL	' XSYS3
	lea	0x18(%rbp),%rbp
	RET
END-CODE

CODE 4XSYS
	CALL	' XSYS4
	lea	0x20(%rbp),%rbp
	RET
END-CODE

CODE 5XSYS
	sub    $0x38,%rsp
	CALL	' XSYS5
	lea	0x28(%rbp),%rbp
	add    $0x38,%rsp
	RET
END-CODE

CODE SSPAGE ( ST -- _ )
	mov    0x40(%rax),%rax
	mov    %rax,%rcx
	$ff C, $50 C, $30 C, \ call   *0x30(%rax)
	RET
END-CODE
