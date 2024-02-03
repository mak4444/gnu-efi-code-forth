
CODE TEST66
	POP %r8
	POP %rAX
 add   (%rax,%rax,2),%edx
	ret
\	$f2 c, $f c, $10 c, 07 c,

END-CODE

see TEST66
