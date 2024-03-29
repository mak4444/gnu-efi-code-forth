
	.data

DEFLAST=0

.macro Wortbirne Name CName FLine Flags
	.quad	\CName
	.long	\Flags,	\FLine
	.quad	DEFLAST	// Link einfügen  Insert Link
	DEFLAST=.
	.byte 8f - 7f	// Länge des Namensfeldes berechnen  Calculate length of name field
7:	.ascii "\Name"    // Namen anfügen  Insert name string
8:
.endm

.macro CONSTANT_W Name Value FLine Flags
	.quad	8f
	.long	\Flags,	\FLine
	.quad	DEFLAST	// Link einfügen  Insert Link
	DEFLAST=.
	.byte 8f - 7f	// Länge des Namensfeldes berechnen  Calculate length of name field
7:	.ascii "\Name"	// Namen anfügen  Insert name string
8:
	call	DOCONSTANT
	.quad	\Value
.endm

.macro VARIABLE_W Name Value FLine Flags
	.quad	8f
	.long	\Flags,	\FLine
	.quad	DEFLAST	// Link einfügen  Insert Link
	DEFLAST=.
	.byte 8f - 7f	// Länge des Namensfeldes berechnen  Calculate length of name field
7:	.ascii "\Name"	// Namen anfügen  Insert name string
8:
\Name:
	call	DOCREATE
	.quad	\Value
.endm

SVIEWLINK=0

.macro VIEW_S Flags
	.quad	SVIEWLINK,0
	SVIEWLINK=.-16
.endm

.macro S_VIEW_PATH Path
	.quad	SVIEWLINK,-1
	SVIEWLINK=.-16
	.byte 8f - 7f	// Länge des Namensfeldes berechnen  Calculate length of name field
7:	.ascii "\Path"	// Namen anfügen  Insert name string
8:
.endm

.macro S_CALL adr
        mov     %rsp, %rbx
	and	$0xf, %rbx
        jne      0f
	push	$NOOP    // for mmx
0:
	push	%rbp
	mov     %rsp, %rbp
        call   \adr
	pop	%rbp

.endm

DROP = DgreatS
_2SWAP	= CSsubSWAP
RSHIFT = greatgreat
LSHIFT = lessless
_1add  = CHARadd
INVERT = NOT
_0save = OFF
INVERT = NOT
PERFORM = loadEXECUTE
RDROP = RgreatDROP


	.global	fimg
fimg:	//.inst	0x777,0x777,0x777,0x777
IMGLIT:

.include "Forth64S/src/src_PRIMITIVES.S"
.include "Forth64S/src/src_VARS.S"
.include "Forth64S/src/src_HPROC.S"
.include "Forth64S/src/macroopt.S"
.include "Forth64S/src/NUMB_PARSE_IO.S"
.include "Forth64S/src/LITERAL.S"
.include "Forth64S/src/src_FIND_INTERP.S"
.include "Forth64S/src/TERM/INCLUDE.S"

.include "Forth64S/src/fstart.S"

	.quad	8f
	.quad	0	// flags
	.quad	0	// Link einfügen  Insert Link
	DEFLAST=.
9:	.byte 8f - 7f     // Länge des Namensfeldes berechnen  Calculate length of name field
7:	.ascii "IMGLIT"    // Namen anfügen  Insert name string
8:	call	DOCONSTANT
	.quad	IMGLIT

CONSTANT_W "LASTSCANCODE" LastScanCode 0 0

CONSTANT_W "&SYSTAB" ST 0 0
CONSTANT_W "&VOLUME" Volume 0 0
CONSTANT_W "&IMAGEHANDLE" LibImageHandle 0 0

.include "Forth64S/src/GHEADERS.S"

ORIGVIEWLINK:
	call	DOCONSTANT
	.quad	SVIEWLINK

FORTH_ORIG:
	call	DOCONSTANT
	.quad	DEFLAST

FORTHsubWORDLIST:
	call	DOCREATE
1:	.quad	DEFLAST
2:	.quad	0	# link
	.quad	0f
0:	.byte	5
	.ascii	"FORTH"   
VOCsubLIST:
	call	DOCREATE
	.quad	2b

CONTEXT_SIZE: 
	call	DOCONSTANT
	.quad	1f - 0f

CONTEXT:
	call DOCREATE
0:	.quad FORTHsubWORDLIST + 5 
	.zero	8*15
1:      .quad	0

CURRENT:
// This variable holds the address of the word list to which new definitions are added.
	call DOCREATE
	.quad FORTHsubWORDLIST + 5 

	.globl start4th_m
start4th_m:
	push %rbp
        lea 0x100000(%rdi),%rbp
	mov %rdi,%rax
	call	FMAIN
	pop  %rbp
	ret
	
