//
//1 VALUE R/O ( -- fam ) \ 94 FILE

RrslO:
	call	DOVALUE	# 8040fb7c e8 42 4b ff ff 

	.quad 0x1
//\ fam - 
//\ 
//
//2 VALUE W/O ( -- fam ) \ 94 FILE

WrslO:
	call	DOVALUE	# 8040fb89 e8 35 4b ff ff 

	.quad 0x2
//\ fam - 
//\ 
//
//3 VALUE R/W ( -- fam ) \ 94 FILE

RrslW:
	call	DOVALUE	# 8040fb96 e8 28 4b ff ff 

	.quad 0x3
//
//: CFNAME-SET  ( adr len -- )
// DUP 1+ >R  CURFILENAME CURFILENAME R@ + CFNAME_SIZE R> - CMOVE>

CFNAMEsubSET:
/*	mov	%rax,-0x08(%rbp)	# 8040fba3 	*/.byte 0x48,0x89,0x45,0xF8
	lea	0x01(%rax),%rax	# 8040fba7 48 8d 40 1 
	Push	%rax	# 8040fbab 50 
	lea	xc+0x14 (%rip),%rax	# 8040fbac 48 8d 5 98 e3 ff ff 
/*	mov	%rax,-0x10(%rbp)	# 8040fbb3 	*/.byte 0x48,0x89,0x45,0xF0
	lea	xc+0x14 (%rip),%rax	# 8040fbb7 48 8d 5 8d e3 ff ff 
/*	add	(%rsp),%rax	# 8040fbbe 	*/.byte 0x48,0x3,0x4,0x24
/*	mov	%rax,-0x18(%rbp)	# 8040fbc2 	*/.byte 0x48,0x89,0x45,0xE8
/*	movq	$0x1BC,-0x20(%rbp)	# 8040fbc6 	*/.byte 0x48,0xC7,0x45,0xE0,0xBC,0x1,0x0,0x0
	pop	%rax	# 8040fbce 58 
/*		negq	%rax	# 8040fbcf 	*/.byte 0x48,0xF7,0xD8
/*	add	-0x20(%rbp),%rax	# 8040fbd2 	*/.byte 0x48,0x3,0x45,0xE0
	lea	-0x18(%rbp),%rbp	# 8040fbd6 48 8d 6d e8 
	call	CMOVEgreat	# 8040fbda e8 a9 4a ff ff 
//CURFILENAME $! ;
/*	mov	%rax,-0x08(%rbp)	# 8040fbdf 	*/.byte 0x48,0x89,0x45,0xF8
	lea	xc+0x14 (%rip),%rax	# 8040fbe3 48 8d 5 61 e3 ff ff 
	lea	-0x08(%rbp),%rbp	# 8040fbea 48 8d 6d f8 
	call	dolsave	# 8040fbee e8 6e 54 ff ff 
	ret		# 8040fbf3 c3 
//
//: CFNAME-FREE  ( -- )
// CURFILENAME COUNT + CURFILENAME


CFNAMEsubFREE:
/*	mov	%rax,-0x08(%rbp)	# 8040fbf4 	*/.byte 0x48,0x89,0x45,0xF8
	lea	xc+0x14 (%rip),%rax	# 8040fbf8 48 8d 5 4c e3 ff ff 
/*	movzbq	(%rax),%rdx	# 8040fbff 	*/.byte 0x48,0xF,0xB6,0x10
	lea	0x01(%rdx,%rax,1),%rax	# 8040fc03 48 8d 44 2 1 
/*	mov	%rax,-0x10(%rbp)	# 8040fc08 	*/.byte 0x48,0x89,0x45,0xF0
	lea	xc+0x14 (%rip),%rax	# 8040fc0c 48 8d 5 38 e3 ff ff 
/*	mov	%rax,-0x18(%rbp)	# 8040fc13 	*/.byte 0x48,0x89,0x45,0xE8
// CFNAME_SIZE CURFILENAME C@ - 255 +  CMOVE ;
/*	movq	$0x1BC,-0x20(%rbp)	# 8040fc17 	*/.byte 0x48,0xC7,0x45,0xE0,0xBC,0x1,0x0,0x0
	lea	xc+0x14 (%rip),%rax	# 8040fc1f 48 8d 5 25 e3 ff ff 
/*	movzb	(%rax),%eax	# 8040fc26 	*/.byte 0xF,0xB6,0x0
/*		negq	%rax	# 8040fc29 	*/.byte 0x48,0xF7,0xD8
/*	add	-0x20(%rbp),%rax	# 8040fc2c 	*/.byte 0x48,0x3,0x45,0xE0
	lea	0xFF(%rax),%rax	# 8040fc30 48 8d 80 ff 0 0 0 
	lea	-0x18(%rbp),%rbp	# 8040fc37 48 8d 6d e8 
	call	CMOVE	# 8040fc3b e8 6c 4a ff ff 
	ret		# 8040fc40 c3 
//
//\ : VVV #H. >PF >DPF ;
//
//: INCLUDE-FILE ( i*x fileid -- j*x ) \ 94 FILE
//\	." INC"
//	>IN  @ >R


INCLUDEsubFILE:
/*	mov	%rax,-0x08(%rbp)	# 8040fc41 	*/.byte 0x48,0x89,0x45,0xF8
/*	push	UNLOOP+0x226 (%rip)	# 8040fc45 	*/.byte 0xFF,0x35,0x3E,0x4E,0xFF,0xFF
//	CURSTR @ >R CURSTR 0!
/*	push	OKdot+0xd (%rip)	# 8040fc4b 	*/.byte 0xFF,0x35,0xF2,0xE0,0xFF,0xFF
/*	movq	$0x0,OKdot+0xd (%rip)	# 8040fc51 	*/.byte 0x48,0xC7,0x5,0xE7,0xE0,0xFF,0xFF,0x0,0x0,0x0,0x0
/*	push	SOURCEsubID+0x5 (%rip)	# 8040fc5c 	*/.byte 0xFF,0x35,0x50,0xD3,0xFF,0xFF
//	SOURCE-ID >R  TO SOURCE-ID
	lea	SOURCEsubID+0x5 (%rip),%rax	# 8040fc62 48 8d 5 49 d3 ff ff 
/*	mov	-0x08(%rbp),%rdx	# 8040fc69 	*/.byte 0x48,0x8B,0x55,0xF8
/*	mov	%rdx,(%rax)	# 8040fc6d 	*/.byte 0x48,0x89,0x10
/*	mov	0x00(%rbp),%rax	# 8040fc70 	*/.byte 0x48,0x8B,0x45,0x0
/*	mov	TIB+0x5 (%rip),%rax	# 8040fc74 	*/.byte 0x48,0x8B,0x5,0x44,0xD3,0xFF,0xFF
//\	CR RP@ 6 >> $F AND SPACES ." save=<" SOURCE TYPE ." >"
//	SOURCE
/*	mov	%rax,-0x08(%rbp)	# 8040fc7b 	*/.byte 0x48,0x89,0x45,0xF8
/*	mov	UNLOOP+0x256 (%rip),%rax	# 8040fc7f 	*/.byte 0x48,0x8B,0x5,0x33,0x4E,0xFF,0xFF
	lea	-0x08(%rbp),%rbp	# 8040fc86 48 8d 6d f8 
//	RP@ OVER ALIGNED - RP!
	call	RPload	# 8040fc8a e8 75 44 ff ff 
/*	mov	%rax,-0x08(%rbp)	# 8040fc8f 	*/.byte 0x48,0x89,0x45,0xF8
/*	mov	0x00(%rbp),%rax	# 8040fc93 	*/.byte 0x48,0x8B,0x45,0x0
	lea	-0x08(%rbp),%rbp	# 8040fc97 48 8d 6d f8 
	call	ALIGNED	# 8040fc9b e8 1 52 ff ff 
/*		negq	%rax	# 8040fca0 	*/.byte 0x48,0xF7,0xD8
/*	add	0x00(%rbp),%rax	# 8040fca3 	*/.byte 0x48,0x3,0x45,0x0
	lea	0x08(%rbp),%rbp	# 8040fca7 48 8d 6d 8 
	call	RPsave	# 8040fcab e8 6b 44 ff ff 
//	RP@ SWAP CMOVE
	call	RPload	# 8040fcb0 e8 4f 44 ff ff 
/*	mov	0x00(%rbp),%rdx	# 8040fcb5 	*/.byte 0x48,0x8B,0x55,0x0
/*	mov	%rax,0x00(%rbp)	# 8040fcb9 	*/.byte 0x48,0x89,0x45,0x0
/*	mov	%rdx,%rax	# 8040fcbd 	*/.byte 0x48,0x89,0xD0
	call	CMOVE	# 8040fcc0 e8 e7 49 ff ff 
//	SOURCE 2>R 
/*	mov	%rax,-0x08(%rbp)	# 8040fcc5 	*/.byte 0x48,0x89,0x45,0xF8
/*	mov	TIB+0x5 (%rip),%rax	# 8040fcc9 	*/.byte 0x48,0x8B,0x5,0xEF,0xD2,0xFF,0xFF
/*	mov	%rax,-0x10(%rbp)	# 8040fcd0 	*/.byte 0x48,0x89,0x45,0xF0
/*	mov	UNLOOP+0x256 (%rip),%rax	# 8040fcd4 	*/.byte 0x48,0x8B,0x5,0xDE,0x4D,0xFF,0xFF
	lea	-0x10(%rbp),%rbp	# 8040fcdb 48 8d 6d f0 
	call	_2greatR	# 8040fcdf e8 88 44 ff ff 
//	['] FQUIT CATCH	SAVEERR	
/*	mov	%rax,-0x08(%rbp)	# 8040fce4 	*/.byte 0x48,0x89,0x45,0xF8
	lea	FQUIT(%rip),%rax	# 8040fce8 48 8d 5 f5 e1 ff ff 
	lea	-0x08(%rbp),%rbp	# 8040fcef 48 8d 6d f8 
	call	CATCH	# 8040fcf3 e8 ec 56 ff ff 
	call	SAVEERR	# 8040fcf8 e8 c6 e7 ff ff 
//	DUP  IF cr ." in <" CURFILENAME COUNT TYPE ." >:" CURSTR @ . THEN \	['] NOOP CATCH	SAVEERR	
/*	mov	%rax,-0x08(%rbp)	# 8040fcfd 	*/.byte 0x48,0x89,0x45,0xF8
	test	%rax,%rax	# 8040fd01 48 85 c0 
/*	mov	-0x08(%rbp),%rax	# 8040fd04 	*/.byte 0x48,0x8B,0x45,0xF8
/*	jz	INCLUDEsubFILE+0x122 	# 8040fd08 	*/.byte 0xF,0x84,0x55,0x0,0x0,0x0
	call	CR	# 8040fd0e e8 5c 51 ff ff 
	call	cbigSdticcend	# 8040fd13 e8 96 51 ff ff 
	.byte 0x4,0x69,0x6E,0x20,0x3C
	call	TYPE	# 8040fd1d e8 1b 51 ff ff 
/*	mov	%rax,-0x08(%rbp)	# 8040fd22 	*/.byte 0x48,0x89,0x45,0xF8
	lea	xc+0x14 (%rip),%rax	# 8040fd26 48 8d 5 1e e2 ff ff 
	lea	0x01(%rax),%rdx	# 8040fd2d 48 8d 50 1 
/*	movzbq	(%rax),%rax	# 8040fd31 	*/.byte 0x48,0xF,0xB6,0x0
/*	mov	%rdx,-0x10(%rbp)	# 8040fd35 	*/.byte 0x48,0x89,0x55,0xF0
	lea	-0x10(%rbp),%rbp	# 8040fd39 48 8d 6d f0 
	call	TYPE	# 8040fd3d e8 fb 50 ff ff 
	call	cbigSdticcend	# 8040fd42 e8 67 51 ff ff 
	.byte 0x2,0x3E,0x3A
	call	TYPE	# 8040fd4a e8 ee 50 ff ff 
/*	mov	%rax,-0x08(%rbp)	# 8040fd4f 	*/.byte 0x48,0x89,0x45,0xF8
/*	mov	OKdot+0xd (%rip),%rax	# 8040fd53 	*/.byte 0x48,0x8B,0x5,0xE9,0xDF,0xFF,0xFF
	lea	-0x08(%rbp),%rbp	# 8040fd5a 48 8d 6d f8 
	call	dot	# 8040fd5e e8 51 c0 ff ff 
//	2R> SOURCE!
	call	_2Rgreat	# 8040fd63 e8 cc 43 ff ff 
/*	mov	%rax,UNLOOP+0x256 (%rip)	# 8040fd68 	*/.byte 0x48,0x89,0x5,0x4A,0x4D,0xFF,0xFF
	lea	TIB+0x5 (%rip),%rax	# 8040fd6f 48 8d 5 49 d2 ff ff 
/*	mov	0x00(%rbp),%rdx	# 8040fd76 	*/.byte 0x48,0x8B,0x55,0x0
/*	mov	%rdx,(%rax)	# 8040fd7a 	*/.byte 0x48,0x89,0x10
/*	mov	0x08(%rbp),%rax	# 8040fd7d 	*/.byte 0x48,0x8B,0x45,0x8
	lea	0x10(%rbp),%rbp	# 8040fd81 48 8d 6d 10 
//	RP@ SOURCE CMOVE
	call	RPload	# 8040fd85 e8 7a 43 ff ff 
/*	mov	%rax,-0x08(%rbp)	# 8040fd8a 	*/.byte 0x48,0x89,0x45,0xF8
/*	mov	TIB+0x5 (%rip),%rax	# 8040fd8e 	*/.byte 0x48,0x8B,0x5,0x2A,0xD2,0xFF,0xFF
/*	mov	%rax,-0x10(%rbp)	# 8040fd95 	*/.byte 0x48,0x89,0x45,0xF0
/*	mov	UNLOOP+0x256 (%rip),%rax	# 8040fd99 	*/.byte 0x48,0x8B,0x5,0x19,0x4D,0xFF,0xFF
	lea	-0x10(%rbp),%rbp	# 8040fda0 48 8d 6d f0 
	call	CMOVE	# 8040fda4 e8 3 49 ff ff 
//\	CR ." IF=<" SOURCE TYPE ." >"
//	RP@ SOURCE NIP ALIGNED + RP!
	call	RPload	# 8040fda9 e8 56 43 ff ff 
/*	mov	%rax,-0x08(%rbp)	# 8040fdae 	*/.byte 0x48,0x89,0x45,0xF8
/*	mov	UNLOOP+0x256 (%rip),%rax	# 8040fdb2 	*/.byte 0x48,0x8B,0x5,0x0,0x4D,0xFF,0xFF
	lea	-0x08(%rbp),%rbp	# 8040fdb9 48 8d 6d f8 
	call	ALIGNED	# 8040fdbd e8 df 50 ff ff 
/*	add	0x00(%rbp),%rax	# 8040fdc2 	*/.byte 0x48,0x3,0x45,0x0
	lea	0x08(%rbp),%rbp	# 8040fdc6 48 8d 6d 8 
	call	RPsave	# 8040fdca e8 4c 43 ff ff 
//\	CR RP@ 6 >> $F AND SPACES ." rest=<" SOURCE TYPE ." >"
//	R> TO SOURCE-ID
/*	mov	%rax,-0x08(%rbp)	# 8040fdcf 	*/.byte 0x48,0x89,0x45,0xF8
	pop	-0x10(%rbp)	# 8040fdd3 8f 45 f0 
	lea	SOURCEsubID+0x5 (%rip),%rax	# 8040fdd6 48 8d 5 d5 d1 ff ff 
/*	mov	-0x10(%rbp),%rdx	# 8040fddd 	*/.byte 0x48,0x8B,0x55,0xF0
/*	mov	%rdx,(%rax)	# 8040fde1 	*/.byte 0x48,0x89,0x10
	pop	-0x10(%rbp)	# 8040fde4 8f 45 f0 
	lea	OKdot+0xd (%rip),%rax	# 8040fde7 48 8d 5 55 df ff ff 
//	R> CURSTR !
/*	mov	-0x10(%rbp),%rdx	# 8040fdee 	*/.byte 0x48,0x8B,0x55,0xF0
/*	mov	%rdx,(%rax)	# 8040fdf2 	*/.byte 0x48,0x89,0x10
	pop	-0x10(%rbp)	# 8040fdf5 8f 45 f0 
	lea	UNLOOP+0x226 (%rip),%rax	# 8040fdf8 48 8d 5 8a 4c ff ff 
//	R> >IN ! THROW  
/*	mov	-0x10(%rbp),%rdx	# 8040fdff 	*/.byte 0x48,0x8B,0x55,0xF0
/*	mov	%rdx,(%rax)	# 8040fe03 	*/.byte 0x48,0x89,0x10
/*	mov	-0x08(%rbp),%rax	# 8040fe06 	*/.byte 0x48,0x8B,0x45,0xF8
	call	THROW	# 8040fe0a e8 2e 56 ff ff 
//;
	ret		# 8040fe0f c3 
//
//T: VIEW_{
//?YDP><DP


VIEW_lsk:
	call	queYDPgreatlessDP	# 8040fe10 e8 4a f2 ff ff 
//   HERE VIEW_LINK , TO VIEW_LINK  0 ,
/*	mov	%rax,-0x08(%rbp)	# 8040fe15 	*/.byte 0x48,0x89,0x45,0xF8
/*	mov	HsubSTDOUT+0x15 (%rip),%rax	# 8040fe19 	*/.byte 0x48,0x8B,0x5,0xF2,0x4E,0xFF,0xFF
/*	mov	%rax,dcomasubSET+0x5 (%rip)	# 8040fe20 	*/.byte 0x48,0x89,0x5,0x43,0x59,0xFF,0xFF
/*	mov	%rax,-0x10(%rbp)	# 8040fe27 	*/.byte 0x48,0x89,0x45,0xF0
/*	mov	VIEW_LINK+0x5 (%rip),%rax	# 8040fe2b 	*/.byte 0x48,0x8B,0x5,0x50,0xF2,0xFF,0xFF
	lea	-0x10(%rbp),%rbp	# 8040fe32 48 8d 6d f0 
	call	com	# 8040fe36 e8 17 4f ff ff 
/*	mov	%rax,VIEW_LINK+0x5 (%rip)	# 8040fe3b 	*/.byte 0x48,0x89,0x5,0x40,0xF2,0xFF,0xFF
/*	mov	$0x0,%eax	# 8040fe42 	*/.byte 0xB8,0x0,0x0,0x0,0x0
	call	com	# 8040fe47 e8 6 4f ff ff 
//?YDP><DP
	call	queYDPgreatlessDP	# 8040fe4c e8 e f2 ff ff 
//;
	ret		# 8040fe51 c3 
//
//T: }VIEW
//?YDP><DP


rskVIEW:
	call	queYDPgreatlessDP	# 8040fe52 e8 8 f2 ff ff 
//   HERE  VIEW_LINK , TO VIEW_LINK -1 ,
/*	mov	%rax,-0x08(%rbp)	# 8040fe57 	*/.byte 0x48,0x89,0x45,0xF8
/*	mov	HsubSTDOUT+0x15 (%rip),%rax	# 8040fe5b 	*/.byte 0x48,0x8B,0x5,0xB0,0x4E,0xFF,0xFF
/*	mov	%rax,dcomasubSET+0x5 (%rip)	# 8040fe62 	*/.byte 0x48,0x89,0x5,0x1,0x59,0xFF,0xFF
/*	mov	%rax,-0x10(%rbp)	# 8040fe69 	*/.byte 0x48,0x89,0x45,0xF0
/*	mov	VIEW_LINK+0x5 (%rip),%rax	# 8040fe6d 	*/.byte 0x48,0x8B,0x5,0xE,0xF2,0xFF,0xFF
	lea	-0x10(%rbp),%rbp	# 8040fe74 48 8d 6d f0 
	call	com	# 8040fe78 e8 d5 4e ff ff 
/*	mov	%rax,VIEW_LINK+0x5 (%rip)	# 8040fe7d 	*/.byte 0x48,0x89,0x5,0xFE,0xF1,0xFF,0xFF
/*	movq	$-0x1,%rax	# 8040fe84 	*/.byte 0x48,0xC7,0xC0,0xFF,0xFF,0xFF,0xFF
	call	com	# 8040fe8b e8 c2 4e ff ff 
//   CURFILENAME COUNT S",
/*	mov	%rax,-0x08(%rbp)	# 8040fe90 	*/.byte 0x48,0x89,0x45,0xF8
	lea	xc+0x14 (%rip),%rax	# 8040fe94 48 8d 5 b0 e0 ff ff 
	lea	0x01(%rax),%rdx	# 8040fe9b 48 8d 50 1 
/*	movzbq	(%rax),%rax	# 8040fe9f 	*/.byte 0x48,0xF,0xB6,0x0
/*	mov	%rdx,-0x10(%rbp)	# 8040fea3 	*/.byte 0x48,0x89,0x55,0xF0
	lea	-0x10(%rbp),%rbp	# 8040fea7 48 8d 6d f0 
	call	Sdticcom	# 8040feab e8 b2 e8 ff ff 
//?YDP><DP
	call	queYDPgreatlessDP	# 8040feb0 e8 aa f1 ff ff 
//
//;
	ret		# 8040feb5 c3 
//
//T: INCLUDED  ( c-addr u ---- )
//\ Open the file with name c-addr u and interpret all lines contained in it.
//\	CR RP@ 6 >> $F AND SPACES ." TO   <" 2DUP TYPE ." >"
//	2DUP	R/O  OPEN-FILE THROW >R


INCLUDED:
/*	mov	0x00(%rbp),%rdx	# 8040feb6 	*/.byte 0x48,0x8B,0x55,0x0
/*	mov	%rax,-0x08(%rbp)	# 8040feba 	*/.byte 0x48,0x89,0x45,0xF8
/*	mov	%rdx,-0x10(%rbp)	# 8040febe 	*/.byte 0x48,0x89,0x55,0xF0
/*	mov	%rax,-0x18(%rbp)	# 8040fec2 	*/.byte 0x48,0x89,0x45,0xE8
/*	mov	RrslO+0x5 (%rip),%rax	# 8040fec6 	*/.byte 0x48,0x8B,0x5,0xB4,0xFC,0xFF,0xFF
	lea	-0x18(%rbp),%rbp	# 8040fecd 48 8d 6d e8 
	call	OPENsubFILE	# 8040fed1 e8 6a db ff ff 
	call	THROW	# 8040fed6 e8 62 55 ff ff 
	Push	%rax	# 8040fedb 50 
/*	mov	0x00(%rbp),%rax	# 8040fedc 	*/.byte 0x48,0x8B,0x45,0x0
	lea	0x08(%rbp),%rbp	# 8040fee0 48 8d 6d 8 
//	CFNAME-SET
	call	CFNAMEsubSET	# 8040fee4 e8 ba fc ff ff 
//        VIEW_{
	call	VIEW_lsk	# 8040fee9 e8 22 ff ff ff 
//	R@ ['] INCLUDE-FILE
/*	mov	%rax,-0x08(%rbp)	# 8040feee 	*/.byte 0x48,0x89,0x45,0xF8
/*	mov	(%rsp),%rax	# 8040fef2 	*/.byte 0x48,0x8B,0x4,0x24
/*	mov	%rax,-0x10(%rbp)	# 8040fef6 	*/.byte 0x48,0x89,0x45,0xF0
	lea	INCLUDEsubFILE(%rip),%rax	# 8040fefa 48 8d 5 40 fd ff ff 
	lea	-0x10(%rbp),%rbp	# 8040ff01 48 8d 6d f0 
//	CATCH
	call	CATCH	# 8040ff05 e8 da 54 ff ff 
//	}VIEW
	call	rskVIEW	# 8040ff0a e8 43 ff ff ff 
//\	CR RP@ 8 + 6 >> $F AND SPACES  ." FROM <" CURFILENAME COUNT TYPE ." >"  DUP . 
//	CFNAME-FREE
	call	CFNAMEsubFREE	# 8040ff0f e8 e0 fc ff ff 
//	R> CLOSE-FILE DROP
/*	mov	%rax,-0x08(%rbp)	# 8040ff14 	*/.byte 0x48,0x89,0x45,0xF8
	pop	%rax	# 8040ff18 58 
	lea	-0x08(%rbp),%rbp	# 8040ff19 48 8d 6d f8 
	call	CLOSEsubFILE	# 8040ff1d e8 ad 48 ff ff 
/*	mov	0x00(%rbp),%rax	# 8040ff22 	*/.byte 0x48,0x8B,0x45,0x0
	lea	0x08(%rbp),%rbp	# 8040ff26 48 8d 6d 8 
//	THROW
	call	THROW	# 8040ff2a e8 e 55 ff ff 
//;
	ret		# 8040ff2f c3 
//
//
//: FLOAD  PARSE-NAME INCLUDED ;


FLOAD:
	call	PARSEsubNAME	# 8040ff30 e8 f1 d2 ff ff 
	call	INCLUDED	# 8040ff35 e8 7c ff ff ff 
	ret		# 8040ff3a c3 
//
