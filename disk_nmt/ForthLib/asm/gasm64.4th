\ Assembler for x86-64

REQUIRE [AGAIN] ForthLib\lib\twopass.4th

\- ENUM : ENUM ( n -- n+1) DUP CONSTANT 1+ ;

\- DCREATE : DCREATE CREATE ;
\- TCREATE : TCREATE CREATE ;
\- TDOES>  : TDOES> POSTPONE DOES> ; IMMEDIATE

\- <<	: << LSHIFT ;
\- >>	: >> RSHIFT ;
\- A>>	: A>> ARSHIFT ;
\- I,	: I, , ;
\- BREAK	: BREAK  POSTPONE EXIT POSTPONE THEN ; IMMEDIATE
\- CELL/	: CELL/  CELL / ;
\- OR!	:  OR! ( N ADDR -- )	DUP @ ROT OR	SWAP ! ;
\- AND!	: AND! ( N ADDR -- )	DUP @ ROT AND	SWAP ! ;
\- CXOR! :  CXOR! ( N ADDR -- )	DUP C@ ROT XOR	SWAP C! ;
\- WXOR! :  WXOR! ( N ADDR -- )	DUP W@ ROT XOR	SWAP W! ;
\- SHORT? : SHORT? ( n -- -128 < n < 127 )  0x80 + 0x100 U< ;
\- NOT	: NOT INVERT ;
\- M@	: M@ @ ;
\- M!	: M! ! ;
\- MC!	: MC! C! ;
\- LCOUNT : LCOUNT   CELL+ DUP CELL- @ ; 
\ - U>= : U>= U< 0= ;
\- U<= : U<= U> 0= ;
\- 4- : 4- 4 - ;

[IFNDEF] -ROLL
: -ROLL          \ nn-1..n0 nn n -- nn..n0
	?DUP IF ROT >R 1- RECURSE  R>  THEN ;
[THEN]

0 VALUE TEXEC_?
: TEXEC_SET  -1 TO TEXEC_? ;
: TEXEC_DIS  0 TO TEXEC_? ;
\- .S  : .S ( -- )    DEPTH .SN ;

\- 3DUP : 3DUP DUP 2OVER ROT ;

0 VALUE ASMDBG

DECIMAL

: b.??	HERE - 3 << $FFFFE0 AND ;

MODULE: GASM64_MOD

0 VALUE MOREPASS
0 VALUE NPASS

0 VALUE ITERNUM
0xF VALUE DSB_OPT

4 CONSTANT MAXLL

: :LTABLE      ( addr --- )
   TCREATE HERE  MAXLL 0 DO 0 , LOOP  TDOES> \ F7_ED
   DUP
   DUP
 CELL+  MAXLL 1- CELLS CMOVE>
   HERE SWAP ! ;

: BLTABLE      ( --- addr )               \ 
  TCREATE  , TDOES>  \  F7_ED
  @ @
 DUP HERE U> ABORT" backward ref error"

;

: FLTABLE      ( --- addr )               \ 
  TCREATE  , TDOES>  \  F7_ED
  @ -1  SWAP  MAXLL CELLS BOUNDS

  DO  HERE I @ U<
	IF I @ UMIN
	THEN
  CELL
 +LOOP
 DUP -1 =
 IF
   NPASS
	if 1  ABORT" forward ref error or MAXLL not enough" 
	else drop here 4 +
	then
	-1 TO MOREPASS
 THEN
;

: :::L  :LTABLE DUP BLTABLE  FLTABLE  ;

\ ::L  LL0 LL0:
\ LTABLE@ LL0 LTABLE! LL0:
:::L  0: 0B 0F
:::L  1: 1B 1F
:::L  2: 2B 2F
:::L  3: 3B 3F
:::L  4: 4B 4F
:::L  5: 5B 5F
:::L  6: 6B 6F
:::L  7: 7B 7F
:::L  8: 8B 8F
:::L  9: 9B 9F

0 VALUE PARM_HESH

: >PARM_HESH
  PARM_HESH 2 << + TO PARM_HESH
;

: PH: CREATE DUP , 1+ DOES> @ >PARM_HESH ;
1 
PH: %r_x 
PH: $$   
PH: X((
PH: _))
PH: _%rip
DROP

0 VALUE ((OFFSET
0 VALUE #((
0 VALUE REX.RBX
0 VALUE REX_W
1 VALUE REG>8
0 VALUE OSIZE

0 VALUE R_DEPTH
1 VALUE >>))

: REX@
 REX.RBX
 REX_W  OR ;

CREATE <<BUF
   0 C, $40 C, -1 C, $80 C,
  -1 C, -1 C, -1 C, $C0 C,

: ((  ( n -- )  TO ((OFFSET X(( 1 TO #(( ;
: |)) _)) 0 TO #(( ;
: ))  ( n -- )  DEPTH R_DEPTH
\ CR ." ))11 " .S KEY DROP
 = IF 1 THEN 1-
 DUP 7 ANDC IF -333 THROW THEN
\ CR ." ))22 " .S KEY DROP
 <<BUF + C@
 DUP 0< IF -333 THROW THEN
\ CR ." ))33 " .S KEY DROP
 TO >>))  |)) ; 

: %rip _%rip  DEPTH TO R_DEPTH ;

: %_l: CREATE DUP   , 1+ DOES> @ %r_x REX.RBX 2*	TO REX.RBX 0 TO REG>8 ;
: %_x: CREATE DUP   , 1+ DOES> @ %r_x REX.RBX 2*	TO REX.RBX 1 TO OSIZE ;
: %r8b: %_l: DOES> @ %r_x REX.RBX 2* 1+ TO REX.RBX ;

: %e_x: %_l: DOES> @ %r_x REX.RBX 2*    TO REX.RBX 1 TO REG>8 ;
: %r8d: %_l: DOES> @ %r_x REX.RBX 2* 1+ TO REX.RBX 1 TO REG>8 ;

: %r_x: %_l: DOES> @ %r_x REX.RBX 2*    TO REX.RBX  #(( IF DEPTH TO R_DEPTH BREAK $8 TO REX_W 1 TO REG>8 ;
: %r_n: %_l: DOES> @ %r_x REX.RBX 2* 1+
 TO REX.RBX  #(( IF DEPTH TO R_DEPTH BREAK $8 TO REX_W 1 TO REG>8 ;

0
%_l: %al %_l: %cl %_l: %dl %_l: %bl %_l: %ah %_l: %ch %_l: %dh %_l: %bh
DROP

0
%r_x: %rax %r_x: %rcx %r_x: %rdx %r_x: %rbx %r_x: %rsp %r_x: %rbp %r_x: %rsi %r_x: %rdi
DROP

0
%r_n: %r8 %r_n: %r9 %r_n: %r10 %r_n: %r11 %r_n: %r12 %r_n: %r13 %r_n: %r14 %r_n: %r15
DROP


0
%e_x: %eax %e_x: %ecx %e_x: %edx %e_x: %ebx %e_x: %esp %e_x: %ebp %e_x: %esi %e_x: %edi
DROP

0
%_x: %ax %_x: %cx %_x: %dx %_x: %bx %_x: %sp %_x: %bp %_x: %si %_x: %di
DROP

0
%r8d: %r8d %r8d: %r9d %r8d: %r10d %r8d: %r11d %r8d: %r12d %r8d: %r13d %r8d: %r14d %r8d: %r15d
DROP

: PARAM:
 CREATE PARM_HESH ,  0 TO PARM_HESH
 DOES>  @ PARM_HESH =
 ;

 %r_x			PARAM: #%r_x
 %r_x	%r_x		PARAM: #%r_x,%r_x
 $$	%r_x		PARAM: #$,%r_x
\ %_l	%_l		PARAM: #%_l,%_l
 0 (( %r_x |))		PARAM: #(%r_x)
 0 (( %r_x %r_x |))	PARAM: #(%r_x,%r_x)
 0 (( %r_x |)) %r_x	PARAM: #(%r_x),%r_x
 0 (( %r_x %r_x |)) %r_x	PARAM: #(%r_x,%r_x),%r_x

 $$ 0 (( %r_x |))		PARAM: #$,(%r_x)
 $$ 0 (( %r_x %r_x |))		PARAM: #$,(%r_x,%r_x)

 %r_x 0 (( %r_x |)) 	PARAM: #%r_x,(%r_x)
 %r_x 0 (( %r_x %r_x |)) PARAM: #%r_x,(%r_x,%r_x)	
 0 (( %rip |))		PARAM: #(%rip)
 0 (( %rip |))	%r_x	PARAM: #(%rip),%r_x
  %r_x 0 (( %rip |))	PARAM: #%r_x,(%rip)


CREATE TAB_(r,r)	0 C, 2 C, 1 C, 3 C,
CREATE TAB_(r)r		0 C, 4 C, 2 C, 3 C, 4 C, 5 C, 6 C, 7 C,
CREATE TAB_r(r)		0 C, 1 C, 2 C, 3 C, 4 C, 5 C, 6 C, 7 C,

CREATE TAB_(r,r)r	0 C, 4 C, 2 C, 6 C, 1 C, 5 C, 3 C, 7 C,
CREATE TAB_r(r,r)	0 C, 2 C, 1 C, 3 C, 4 C, 6 C, 5 C, 7 C,

: REX,
  REX.RBX
 DUP 7 U>
	IF    -333 THROW THEN
 #(%r_x,%r_x)		IF TAB_(r,r)	+ C@ THEN
 #(%r_x),%r_x 
  	IF TAB_(r)r	+ C@ THEN
 #(%r_x,%r_x),%r_x	IF TAB_(r,r)r	+ C@ THEN
 #%r_x,(%r_x)		IF TAB_r(r)	+ C@ THEN
 #%r_x,(%r_x,%r_x)	IF TAB_r(r,r)	+ C@ THEN
 REX_W OR $40 OR C,
  0 TO REX_W  0 TO REX.RBX
;

: ?OSIZE, OSIZE IF $66 C, 0 TO OSIZE THEN ;

: ?REX,  ?OSIZE,  REX@ IF REX, THEN  ;

: DO|;  ( cod -- )
 ?REX,
  OR  C,
 0 TO PARM_HESH 1 TO REG>8 ;

: |;
 POSTPONE  DO|;
 POSTPONE ;
; IMMEDIATE

: data16	$66 C, ; 

\	6c                   	insb   (%dx),%es:(%rdi)
\	6d                   	insl   (%dx),%es:(%rdi)
\	6e                   	outsb  %ds:(%rsi),(%dx)
\	6f                   	outsl  %ds:(%rsi),(%dx)
\	a4                   	movsb  %ds:(%rsi),%es:(%rdi)
\ c8 05 11 22          	enter  $0x1105,$0x22

: ret		$c3 C, ;
: retq		$c3 C, ;
: nop		$90 C, ;
: cwtl		$98 C, ;
: cltd		$99 C, ;
: fwait 	$9b C, ;
: pushfq	$9c C, ;
: popfq 	$9d C, ;
: sahf  	$9e C, ;
: lahf  	$9f C, ;
: movsl  	$a5 C, ;
: cmpsb  	$a6 C, ;
: cmpsl  	$a7 C, ;
: stosb  	$aa C, ;
: stos  	$ab C, ;
: lodsb  	$ac C, ;
: lods  	$ad C, ;
: scasb  	$ae C, ;
: scas  	$af C, ;
: lret		$cb C, ;
: int3		$cc C, ;
: lock  	$f0 C, ;
: icebp  	$f1 C, ; \ int1
: rep		$f3 C, ;
: hlt		$f4 C, ;
: cmc		$f5 C, ;
: clc		$f8 C, ;
: stc		$f9 C, ;
: cli		$fa C, ;
: sti		$fb C, ;
: cld		$fc C, ;
: std		$fd C, ;

: syscall	$050F W, ;
: clts		$060F W, ;
: invd		$080F W, ;
: wbinvd	$090F W, ;
: ud2		$0B0F W, ;
: femms   	$0e0f W, ;
: wrmsr   	$300f W, ;
: rdtsc   	$310f W, ;
: rdmsr   	$320f W, ;
: rdpmc   	$330f W, ;
: sysenter	$340f W, ; 
: sysexit 	$350f W, ;
: getsec  	$370f W, ;
: emms    	$770f W, ;
: cpuid   	$a20f W, ;
: rsm     	$aa0f W, ;
: ud1     	$b90f W, ; 

: cltq		$9848 W, ;               	
: cqto		$9948 W, ;               	
: lretq		$cb48 W, ;               	

: rex	 	$40 C, ;
: rex.B		$41 C, ;
: rex.X		$42 C, ;
: rex.XB	$43 C, ;
: rex.R		$44 C, ;
: rex.RB	$45 C, ;
: rex.RX	$46 C, ;
: rex.RXB	$47 C, ;
: REX.W		$48 C, ;
: REX_WB	$49 C, ;
: REX_WX	$4a C, ;
: REX_WXB	$4b C, ;
: REX_WR	$4c C, ;
: REX_WRB	$4d C, ;
: REX_WRX	$4e C, ;
: REX_WRXB 	$4f C, ;

: endbr64    $fa1e0ff3 L, ;

: not_  ( c -- cod )
  ?REX,  $f6 REG>8 OR C, ;

: inc, 	?REX, $ff C, $C0 |;
: dec, 	?REX, $ff C, $C8 |;
: bswap,	?REX, $0f C, $C8 |;

: (()),
\ CR ." ((00=)" .S KEY DROP
 #%r_x IF  OR	$C0 DO|; 	BREAK

 #(%rip) IF 5 DO|; ((OFFSET  HERE 4+ - L, BREAK
 #(%r_x)
\ CR ." ((22=)" .S KEY DROP
 IF	>>)) IF 4 DO|; 3 << 5 OR >>)) OR C, ((OFFSET L,	BREAK
	OVER 5 = ((OFFSET OR 
  IF	((OFFSET SHORT?
	IF	$40 OR OVER DO|; 4 = IF $24 C, THEN ((OFFSET C,
	BREAK	$80 OR OVER DO|; 4 = IF $24 C, THEN ((OFFSET L,
  BREAK
     OVER DO|; 4 = IF $24 C, THEN
 BREAK
 #(%r_x,%r_x) 
 IF	2 PICK 5 = \ %rbp
	((OFFSET OR
  IF	((OFFSET SHORT? 
	IF	$44 DO|; 3 << OR >>)) OR C, ((OFFSET C, 
	BREAK	$84 DO|; 3 << OR >>)) OR C, ((OFFSET L, 
  BREAK \ ." #(%r_x,%r_x)=<" CR .S ." >" CR KEY DROP
     4 DO|;  3 << OR >>)) OR C,
 BREAK
   -333 THROW
  ;

: $f6(()), ?REX, $f6 C, (()), ;
: notb,		$10 $f6(()), ;
: negb,		$18 $f6(()), ;
: mulb,		$20 $f6(()), ;
: imulb,	$28 $f6(()), ;
: divb,		$30 $f6(()), ;
: idivb,	$38 $f6(()), ;

: $66f7(()), ?REX, $66f6 REG>8 OR W, (()), ;
: notw,		$10 $66f7(()), ;
: negw,		$18 $66f7(()), ;
: mulw,		$20 $66f7(()), ;
: imulw,	$28 $66f7(()), ;
: divw,		$30 $66f7(()), ;
: idivw,	$38 $66f7(()), ;

: $f7(()), ?REX, $f6 REG>8 OR C, (()), ;
: notl,		$10 $f7(()), ;
: negl,		$18 $f7(()), ;
: mull,		$20 $f7(()), ;
: imull,	$28 $f7(()), ;
: divl,		$30 $f7(()), ;
: idivl,	$38 $f7(()), ;

: not,		$10 $f7(()), ;
: neg,		$18 $f7(()), ;
: mul,		$20 $f7(()), ;
: div,		$30 $f7(()), ;
: idiv,		$38 $f7(()), ;

: notq,		$8 TO REX_W not,	; 
: negq,		$8 TO REX_W neg,	; 
: mulq,		$8 TO REX_W mul,	; 
: imulq,	$48 C, $f7 C, $28 (()), ;
: divq,		$8 TO REX_W div,	; 
: idivq,	$8 TO REX_W idiv,	;  


: $f(()), ?REX, $f C, C, 0 (()), ;
: sldt,		$00 $f(()), ;
: sgdt,		$01 $f(()), ;
: prefetch,	$0d $f(()), ;
: fxsave, 	$ae $f(()), ;

: SET,
	$f C, C,
  #%r_x	IF \ REG>8 IF -333 THROW THEN
 $C0 DO|; BREAK
  0 (()), ;

: seto, 	$90 SET, ;
: setno,	$91 SET, ;
: setb, 	$92 SET, ;
: setae,	$93 SET, ;
: sete, 	$94 SET, ;
: setne,	$95 SET, ;
: setbe,	$96 SET, ;
: seta,		$97 SET, ;
: sets, 	$98 SET, ;
: setns,	$99 SET, ;
: setp, 	$9a SET, ;
: setnp,	$9b SET, ;
: setl, 	$9c SET, ;
: setge,	$9d SET, ;
: setle,	$9e SET, ;
: setg, 	$9f SET, ;

: INCL,  ?REX, $FF C, 0 (()), ;

: DECL,
  #(%r_x,%r_x) IF	?REX, $FF C, 8 (()), BREAK
  8 OR INCL, ;
: INCQ, 	$8 TO REX_W incl, ;
: DECQ, 	$8 TO REX_W decl, ;

: POP,  REX.RBX ?DUP IF $40 OR C, THEN
    #%r_x IF 0 TO  REX.RBX  0 TO  REX_W  $58 DO|; break
  $8f C, 0 (()), ;

: PUSH, REX.RBX ?DUP IF $40 OR C, THEN
    #%r_x IF 0 TO  REX.RBX  0 TO  REX_W  $50 DO|; break
  $ff C, $30 (()), ;

: >PARM ( cfa )  >BODY @ TO PARM_HESH ;

0 VALUE SREX_W


: ADD| ( c --  )
       
  REX_W TO SREX_W  ?REX,
  #$,%r_x  IF   \ $imm reg cod
		2 PICK SHORT?
		IF
			OVER REG>8 OR 0= \  al
			IF  nip 4  DO|;  C,
			BREAK

			REG>8 3 * $80 OR C, $C0 OR DO|;  C,
		BREAK		
		OVER 0= \  ax
		IF	nip
			4 REG>8 OR
		    REG>8 IF DO|; L, BREAK DO|; C,
		BREAK
			$80 REG>8 OR C, $C0
		  XOR  REG>8 IF DO|; L, BREAK DO|; C,
	    BREAK

 #$,(%r_x)	IF  \  $imm reg cod			
			2 PICK SHORT?
			IF	$83 C,	['] #(%r_x) >PARM (()), C,
			BREAK	$81 C,	['] #(%r_x) >PARM (()), L, 
		BREAK

 #$,(%r_x,%r_x)	IF  \  $imm reg cod			
			3 PICK SHORT?
			IF	$83 C,	['] #(%r_x,%r_x) >PARM (()), C,
			BREAK	$81 C,	['] #(%r_x,%r_x) >PARM (()), L, 
		BREAK

\  #%_l,%_l   IF C, SWAP 3 << OR $C0 DO|; BREAK

 REG>8 OR

  #%r_x,%r_x		IF C, SWAP 3 << OR $C0 DO|; BREAK

  #%r_x,(%r_x)		IF C, SWAP 3 << ['] #(%r_x)	>PARM (()), BREAK
  #%r_x,(%r_x,%r_x)	IF C, ROT  3 << ['] #(%r_x,%r_x) >PARM (()), BREAK
  #%r_x,(%rip)		IF C, 3 << ['] #(%rip)	>PARM (()), BREAK

  2 OR

  #(%r_x),%r_x		IF C, 3 << ['] #(%r_x)		>PARM (()), BREAK
  #(%r_x,%r_x),%r_x	IF C, 3 << ['] #(%r_x,%r_x)	>PARM (()), BREAK
  #(%rip),%r_x		IF C, 3 << ['] #(%rip)	>PARM (()), BREAK
  -333 THROW
;

: add,	0x00 ADD| ;
: or,	0x08 ADD| ;
: adc,	0x10 ADD| ;
: sbb,	0x18 ADD| ;
: AND,	0x20 ADD| ;
: sub,	0x28 ADD| ;
: xor,	0x30 ADD| ;
: cmp, 	0x38 ADD| ;

: imul, ?REX,  #%r_x,%r_x	IF $af0f W, SWAP 3 << OR $C0 DO|; BREAK
  #(%r_x),%r_x		IF $0f C, $af ADD|  BREAK
  #(%r_x,%r_x),%r_x	IF $0f C, $af ADD|  BREAK 
	$28 $f7(()), ;

: movabs,
  #%r_x	IF 2DUP AND IF -333 THROW THEN
		?REX,
		DUP 0= IF DROP $A0 ELSE NIP $A2 THEN
		REG>8 	DO|; ,
	BREAK
  REX_W 0= IF -333 THROW THEN
  #$,%r_x  IF REX, $B8 DO|; , BREAK
 -333 THROW
;

: mov,
  REX_W TO SREX_W  ?REX,
  #$,%r_x IF	  \  $imm reg

		OVER 0x80000000 +  0x17FFFFFFF U<
		IF
			SREX_W IF  $C7 C, $C0 DO|; L, BREAK
			REG>8 IF  $b8  DO|; L, BREAK
			$B0 DO|; C,
		BREAK

		SREX_W 0= IF -333 THROW THEN
		$B8 DO|; ,
	  BREAK

 #$,(%r_x)	IF	$c7  C,	['] #(%r_x)		>PARM 0 (()), L,	BREAK
 #$,(%r_x,%r_x) IF	$c7  C, ['] #(%r_x,%r_x)	>PARM 0 (()), L,	BREAK

	0x88 ADD| ;

: LONG? REX_W IF -333 THROW THEN ;

: movq, $8 TO REX_W mov, ;
: movl, LONG? mov, ;

: movb,
  REG>8 IF -333 THROW THEN

  #$,%r_x IF	  \  $imm reg
		$B0 DO|; C,
	  BREAK

  #$,(%r_x)		IF $c7  C, ['] #(%r_x)		>PARM 0 (()), L, BREAK
  #$,(%r_x,%r_x)	IF $c7  C, ['] #(%r_x,%r_x)	>PARM 0 (()), L, BREAK

	0x88 ADD| ;


: CMOV|  
  ?REX,  $F C,  C, 3 <<
  #(%r_x),%r_x		IF ['] #(%r_x)		>PARM (()), BREAK
  #(%r_x,%r_x),%r_x	IF ['] #(%r_x,%r_x)	>PARM (()), BREAK
  -333 THROW
;

: cmovo,	$40 CMOV| ;
: cmovno,	$41 CMOV| ;
: cmovb,	$42 CMOV| ;
: cmovae,	$43 CMOV| ;
: cmove, 	$44 CMOV| ;
: cmovne,	$45 CMOV| ;
: cmovbe,	$46 CMOV| ;
\ : cmova, 	$47 CMOV| ;
: cmovs, 	$48 CMOV| ;
: cmovns,	$49 CMOV| ;
: cmovp, 	$4a CMOV| ;
: cmovnp,	$4b CMOV| ;
: cmovl, 	$4c CMOV| ;
: cmovge,	$4d CMOV| ;
: cmovle,	$4e CMOV| ;
\ : cmovg, 	$4f CMOV| ;

: cmovg, ?REX, $f C, 0x4C ADD| ;
: cmova, ?REX, $f C, 0x44 ADD| ;

\ : movl   $0x0,0x0(%rbp)

: LOMG? REX_W IF -333 THROW THEN ;

: movzb, ?REX, 0 TO REG>8 $f C, $B6 ADD| ;
: movzbq, $8 TO REX_W movzb, ;
: movzbl, LOMG?  movzb, ;

: movzw, ?REX, 0 TO REG>8 $f C, $B7 ADD| ;
: movzwq, $8 TO REX_W movzw, ;
: movzwl, LOMG?  movzw, ;

: movsb, PARM_HESH 0= IF 0 $a4 DO|; BREAK
 ?REX, 0 TO REG>8 $f C, $BE ADD| ;
: movsbq, $8 TO REX_W movsb, ;

\ : movsbl, LOMG?  movsb, ;

: movsw, ?REX, 0 TO REG>8 $f C, $BF ADD| ;
: movswq, #%r_x,%r_x IF 0 TO OSIZE THEN $8 TO REX_W movsw, ;
\ : movswl, LOMG?  movsw, ;

: movslq, 0x62 ADD| ;

: lss,	$b2 CMOV| ;
: lfs,	$b4 CMOV| ;
: lgs,	$b5 CMOV| ;

: bsf,		$f C, $bc CMOV| ;
: bsr,		$f C, $bd CMOV| ;
: movsbl,	$f C, $be CMOV| ;
: movswl,	$f C, $bf CMOV| ;

\ : btr,	$f C, $b3 CMOV| ;

: xchg,
  #%r_x,%r_x		IF DUP	0= IF DROP	$90 DO|; BREAK
			   OVER	0= IF NIP	$90 DO|; BREAK
			0x86 ADD|  BREAK
  #%r_x,(%r_x)		IF 0x86 ADD|  BREAK
  #%r_x,(%r_x,%r_x)	IF 0x86 ADD|  BREAK 

  #(%r_x),%r_x		IF 0x84 ADD|  BREAK
  #(%r_x,%r_x),%r_x	IF 0x84 ADD|  BREAK 

\  #%r_x,(%r_x)		IF SWAP ['] #(%r_x),%r_x	>PARM 0x84 ADD| BREAK
\  #%r_x,(%r_x,%r_x)	IF  ROT ['] #(%r_x,%r_x),%r_x	>PARM 0x84 ADD| BREAK 
  -333 THROW
 ;

: test,

  #%r_x,%r_x		IF 0x84 ADD|  BREAK

  #%r_x,(%r_x)		IF 0x84 ADD|  BREAK
  #%r_x,(%r_x,%r_x)	IF 0x84 ADD|  BREAK 

  #(%r_x),%r_x		IF 0x86 ADD|  BREAK
  #(%r_x,%r_x),%r_x	IF 0x86 ADD|  BREAK 

  REX_W TO SREX_W  ?REX,

  #$,%r_x IF
		DUP 0= \  ax
		IF	REG>8	IF $a9 DO|; L,
				BREAK $a8 DO|; C,
		BREAK $C0
			REG>8	IF $F7 C, DO|; L,
				BREAK $F6 C, DO|; C,
	   BREAK

 #$,(%r_x)	IF $F7  C, ['] #(%r_x)		>PARM 0 (()), L,	BREAK
 #$,(%r_x,%r_x)	IF $F7  C, ['] #(%r_x,%r_x)	>PARM 0 (()), L,	BREAK

  -333 THROW
 ;

: lea,
  #(%r_x),%r_x		IF SWAP [']  #%r_x,(%r_x)	>PARM 0x8C ADD|  BREAK
  #(%r_x,%r_x),%r_x	IF -ROT [']  #%r_x,(%r_x,%r_x)	>PARM 0x8C ADD|  BREAK 
  #(%rip),%r_x		IF	[']  #%r_x,(%rip)	>PARM 0x8D ADD|  BREAK
  -333 THROW ;


: SH,	?REX,
	#%r_x	IF	$D0 REG>8 OR C, DO|;	BREAK
	#$,%r_x	IF	$C0 REG>8 OR C, DO|; C,	BREAK
	#%r_x,%r_x
		IF 	ROT ['] %cl >BODY @ <> IF -3333 THROW THEN
			$D2 REG>8 OR C, DO|;	BREAK
 #$,(%r_x)	IF	$C0 REG>8 OR C,	['] #(%r_x)		>PARM $3F AND (()), C,    BREAK
 #$,(%r_x,%r_x)	IF	$C0 REG>8 OR C,	['] #(%r_x,%r_x)	>PARM $3F AND (()), C,    BREAK
  -333 THROW ;

: rol, $c0 SH, ;
: ror, $c8 SH, ;
: rcl, $d0 SH, ;
: rcr, $d8 SH, ;
: shl, $e0 SH, ;
: shr, $e8 SH, ;
: sal, $f0 SH, ;
: sar, $f8 SH, ;

: SHB,  0 TO REG>8 SH, ;

: rolb, $c0 SHB, ;
: rorb, $c8 SHB, ;
: rclb, $d0 SHB, ;
: rcrb, $d8 SHB, ;
: shlb, $e0 SHB, ;
: shrb, $e8 SHB, ;
: salb, $f0 SHB, ;
: sarb, $f8 SHB, ;

: SHW,  66 C, SH, ;

: rolw, $c0 SHW, ;
: rorw, $c8 SHW, ;
: rclw, $d0 SHW, ;
: rcrw, $d8 SHW, ;
: shlw, $e0 SHW, ;
: shrw, $e8 SHW, ;
: salw, $f0 SHW, ;
: sarw, $f8 SHW, ;

: roll, LOMG? rol, ;
: rorl, LOMG? ror, ;
: rcll, LOMG? rcl, ;
: rcrl, LOMG? rcr, ;
: shll, LOMG? shl, ;
: shrl, LOMG? shr, ;
: sall, LOMG? sal, ;
: sarl, LOMG? sar, ;

: rolq, $8 TO REX_W rol, ;
: rorq, $8 TO REX_W ror, ;
: rclq, $8 TO REX_W rcl, ;
: rcrq, $8 TO REX_W rcr, ;
: shlq, $8 TO REX_W shl, ;
: shrq, $8 TO REX_W shr, ;
: salq, $8 TO REX_W sal, ;
: sarq, $8 TO REX_W sar, ;

: btc,	?REX,
	#$,%r_x	IF $ba0f W, $f8 DO|; C,	BREAK

  -333 THROW
;

: CALL,   #%r_x IF 0 TO REX.RBX 0 TO REX_W $FF C, $D0 DO|; BREAK
	PARM_HESH IF $ff C, $10 (()), BREAK
  $E8  C, 4 - HERE - L,  ;

: #LP, C, 1- HERE - C, ;

: JMP,   #%r_x IF 0 TO REX.RBX 0 TO REX_W $FF C, $E0 DO|; BREAK
	PARM_HESH IF $ff C, $20 (()), BREAK
  OVER 1- HERE - SHORT? IF $eb #LP, BREAK
  $E9  C, 4 - HERE - L,  ;

: loopne,	$e0 #LP, ;
: loope,	$e1 #LP, ;
: loop,		$e2 #LP, ;
: jrcxz,	$e3 #LP, ;

$75 constant Z?     $74 constant NZ?    $79 constant S?
$78 constant NS?    $7D constant L?     $7C constant GE?
$7F constant LE?    $7E constant G?     $73 constant CY?
$72 constant NC?    $77 constant BE?    $76 constant A?
$71 constant O?     $70 constant NO?    $7A constant PO?
$7B constant PE?    $72 constant AE?    $73 constant B?
$E3 constant NCXZ?  $72 constant NCY?   $EB constant NV? ( never )

: J?   OVER 1- HERE - SHORT? IF $70 OR #LP, BREAK
		$f C,  $80 + C, 4 - HERE - L,  ;
: jo,  	$0 J? ;
: jno, 	$1 J? ;
: jb,  	$2 J? ;
: jae, 	$3 J? ;
: je,  	$4 J? ;
: jz,  	$4 J? ;
: jne, 	$5 J? ;
: jbe, 	$6 J? ;
: ja,  	$7 J? ;
: js,  	$8 J? ;
: jns, 	$9 J? ;
: jp,  	$a J? ;
: jnp, 	$b J? ;
: jl,  	$c J? ;
: jge, 	$d J? ;
: jle, 	$e J? ;
: jg,  	$f J? ;

0 constant con-flg		\ -- 0 ; address structure flag

: ?>mark        \ -- as-flag as-orig
\ lay forward branch offset after opcode; patched up later
  con-flg here  0 c,
;

: if,		\ cond -- as-flag as-orig
  c, ?>mark	\ preserve condition, and do last opcode
;

: then,		\ as-flag as-orig --
  here over 1+ -  dup SHORT? 0= IF 333 THROW THEN
  swap c!  con-flg XOR  IF 333 THROW THEN
;

: endif,	\ as-flag as-orig --
  then,
;

: else,		\ as-flag1 as-orig1 -- as-flag2 as-orig2
  $0EB if,  2swap  then,	\ magic number for JMP rel8
;

: begin,	\ -- as-flag as-dest
  con-flg  here
;

: until,	\ as-flag as-dest cond --
   c,
  here 1+ - dup SHORT? 0= IF 333 THROW THEN
 con-flg XOR  IF 333 THROW THEN
;

: again,	\ as-flag as-dest --
  $0EB until,
;

: while,	\ as-flag as-dest cond -- as-flag as-orig as-flag as-dest
  if, 2swap
;

: repeat,	\ as-flag as-orig as-flag as-dest --
  again,  then,
;

: times,	\ n -- dest
  $$  %rcx mov,  here	\ points after  mov  rcx, # n
;

CREATE NLASTS 0 ,
0 VALUE TEB_CLEAN

: CODL
\+ :#THS   :#THS
	TEXEC_?
	IF
[IFDEF] TEXEC_BUF
	>IN M@ >R
	PARSE-NAME
 SFIND

	IF
			TEB_CLEAN CELL+ TO TEB_CLEAN
			TEXEC_BUF TEXEC_BUF CELL+ TEB_SIZE MOVE
			TEXEC_KEY TEXEC_KEY CELL+ TEB_SIZE MOVE
			TEXEC_BUF M!
		HERE	TEXEC_KEY M!
	ELSE	2DROP
	THEN

	R> >IN M!
[THEN]  THEN
	HEADER

        NLASTS 1+!
;

DEFER A?HEAD

' NOOP TO A?HEAD

\+ HERE-TAB-CUR VARIABLE HERE-TAB-CUR-SAVE

\+ HERE-TAB-CUR VARIABLE SHERE-TAB-CUR-SAVE

0 VALUE MOREPASSFLG
0 VALUE SAVELAST

: ASM_END
 MOREPASS TO MOREPASSFLG
 MOREPASS IF
\ CR ." MOREPASS"
\+ HERE-TAB-CUR 	HERE-TAB-CUR-SAVE @ HERE-TAB-CUR !
\+ HERE-TAB-CUR 	SHERE-TAB-CUR-SAVE @ SHERE-TAB-CUR !
  ITERNUM 2+ TO   ITERNUM
   NPASS 1+ TO NPASS
\  redundant items delete
\+ TEXEC_BUF 	TEXEC_BUF TEB_CLEAN + TEXEC_BUF TEB_SIZE  MOVE
\+ TEXEC_BUF 	TEXEC_KEY TEB_CLEAN + TEXEC_KEY TEB_SIZE  MOVE
	0 TO TEB_CLEAN
  0 TO MOREPASS
  SAVELAST CURRENT M@ M!
   POSTPONE [AGAIN]
   EXIT
 THEN
	PREVIOUS
\+ SHERE-TO-TAB   SHERE-TO-TAB

;

EXPORT

\- T-ALIGN : T-ALIGN  BEGIN  HERE 3 AND WHILE 0xFF C, REPEAT ;

: GASM_BIG
\+ SHERE-TO-TAB  SHERE-TO-TAB

	ALSO GASM64_MOD
  0 TO MOREPASS
  0 TO NPASS
  0 TO TEB_CLEAN

  CURRENT M@ M@ TO SAVELAST

  ITERNUM 2+ TO   ITERNUM

\+ HERE-TAB-CUR 	HERE-TAB-CUR @ HERE-TAB-CUR-SAVE !
\+ HERE-TAB-CUR 	SHERE-TAB-CUR @ SHERE-TAB-CUR-SAVE !
 POSTPONE [BEGIN]

 ;

: CODE
    CODL
    GASM_BIG \    (CODE)
  ;

;MODULE

FLOAD ForthLib/asm/lex.4th 
FLOAD ForthLib/asm/prefix.4th 

