\ Assembler for Cortex

\ REQUIRE  HERE-TAB-CUR ~mak\lib\THERE\mlist.f 
REQUIRE [IF] ~mak/CompIF1.f
REQUIRE [IFNDEF] ~nn/lib/ifdef.f

REQUIRE NUMBER? ~mak/lib/fpcnum.f

REQUIRE [AGAIN] ~mak/lib/twopass.f 

[IFNDEF] \+
: \+    POSTPONE [DEFINED] 0= IF POSTPONE \ THEN ; IMMEDIATE
: \-    POSTPONE [DEFINED]    IF POSTPONE \ THEN ; IMMEDIATE
[THEN]      
 
\- <<	: << LSHIFT ;
\- >>	: >> RSHIFT ;
\- BREAK	: BREAK  POSTPONE EXIT POSTPONE THEN ; IMMEDIATE
\- CELL/	: CELL/  CELL / ;
\- OR!	:  OR! ( N ADDR -- )	DUP @ ROT OR	SWAP ! ;
\- AND!	: AND! ( N ADDR -- )	DUP @ ROT AND	SWAP ! ;
\- SHORT? : SHORT? ( n -- -128 < n < 127 )  0x80 + 0x100 U< ;
\- NOT	: NOT INVERT ;

[IFNDEF] .S  : .S ( -- )    DEPTH .SN ;  [THEN]

[IFNDEF] 3DUP : 3DUP DUP 2OVER ROT ;  [THEN]

0 VALUE ASMDBG

MODULE: ASM64_MOD

0 VALUE MOREPASS
0 VALUE NPASS

0 VALUE ITERNUM

0xF VALUE DSB_OPT

: LTABLE@      ( --- addr )               \ 
   CREATE HERE 0 , 0 , DOES> \ F7_ED
 DUP CELL+ @ 1 NOT AND
 ITERNUM  <>
 IF ITERNUM
 OVER CELL+ !
 THEN  @
;

: LTABLE!      ( addr --- )               \ 
   CREATE ,   DOES> @
 DUP  CELL+ @ ITERNUM 1+ = ABORT" already defined"
 DUP  CELL+ @ ITERNUM =
  IF   DUP @ HERE <> IF -1 TO MOREPASS THEN
	ITERNUM 1+ OVER CELL+ !
  THEN   HERE SWAP ! ;

: ::L  LTABLE@  LTABLE!  ;

::L  LL0 LL0:
::L  LL1 LL1:
::L  LL2 LL2:
::L  LL3 LL3:
::L  LL4 LL4:
::L  LL5 LL5:
::L  LL6 LL6:
::L  LL7 LL7:
::L  LL8 LL8:
::L  LL9 LL9:
::L  LLA LLA:
::L  LLB LLB:
::L  LLC LLC:
::L  LLD LLD:
::L  LLE LLE:
::L  LLF LLF:


0 VALUE PARM_HESH

: >PARM_HESH
  PARM_HESH 2 << + TO PARM_HESH
;

: PH: CREATE DUP , 1+ DOES> @ >PARM_HESH ;
1 
PH: %_l
PH: %r_x 
PH: $$   
PH: X((
PH: _))
DROP

0 VALUE ((OFFSET
0 VALUE #((
0 VALUE REX.RBX
0 VALUE REX_W
0 VALUE REG>8

: REX
 REX.RBX
 REX_W  OR ;

: ((  ( n -- )  TO ((OFFSET X(( 1 TO #(( ;
: ))  ( n -- )  _)) 0 TO #(( ;
 
: %_l: CREATE DUP   , 1+ DOES> @ %r_x REX.RBX 2*    TO REX.RBX ;
: %r8b: %_l: DOES> @ %r_x REX.RBX 2* 1+ TO REX.RBX ;

: %e_x: %_l: DOES> @ %r_x REX.RBX 2*    TO REX.RBX 1 TO REG>8 ;
: %r8d: %_l: DOES> @ %r_x REX.RBX 2* 1+ TO REX.RBX 1 TO REG>8 ;

: %r_x: %_l: DOES> @ %r_x REX.RBX 2*    TO REX.RBX #(( IF BREAK $8 TO REX_W 1 TO REG>8 ;
: %r_n: %_l: DOES> @ %r_x REX.RBX 2* 1+ TO REX.RBX #(( IF BREAK $8 TO REX_W 1 TO REG>8 ;


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
%r8d: %r8d %r8d: %r9d %r8d: %r10d %r8d: %r11d %r8d: %r12d %r8d: %r13d %r8d: %r14d %r8d: %r15d
DROP


: PARAM:
 CREATE PARM_HESH ,  0 TO PARM_HESH
 DOES>  @ PARM_HESH =
 ;

: PARAM.:
 CREATE PARM_HESH ,  0 TO PARM_HESH
 DOES>  @ PARM_HESH =
 ;
                               
 %r_x			PARAM: #%r_x
 %r_x	%r_x		PARAM: #%r_x,%r_x
 $$	%r_x		PARAM: #$,%r_x
\ %_l	%_l		PARAM: #%_l,%_l
 0 (( %r_x ))		PARAM: #(%r_x)
 0 (( %r_x %r_x ))	PARAM: #(%r_x,%r_x)
 0 (( %r_x )) %r_x	PARAM.: #(%r_x),%r_x
 0 (( %r_x %r_x )) %r_x	PARAM: #(%r_x,%r_x),%r_x

 %r_x 0 (( %r_x )) 	PARAM: #%r_x,(%r_x)
 %r_x 0 (( %r_x %r_x )) PARAM: #%r_x,(%r_x,%r_x)	


CREATE TAB_(r,r)	0 C, 2 C, 1 C, 3 C,
CREATE TAB_(r)r		0 C, 1 C, 2 C, 3 C, 4 C, 5 C, 6 C, 7 C,
CREATE TAB_r(r)		0 C, 1 C, 2 C, 3 C, 4 C, 5 C, 6 C, 7 C,

CREATE TAB_(r,r)r	0 C, 4 C, 2 C, 6 C, 1 C, 5 C, 3 C, 7 C,
CREATE TAB_r(r,r)	0 C, 2 C, 1 C, 3 C, 4 C, 6 C, 5 C, 7 C,

: REX,
  REX.RBX DUP 7 U>	IF    -333 THROW THEN
 #(%r_x,%r_x)		IF TAB_(r,r)	+ C@ THEN
 #(%r_x),%r_x   	IF TAB_(r)r	+ C@ THEN
 #(%r_x,%r_x),%r_x	IF TAB_(r,r)r	+ C@ THEN
 #%r_x,(%r_x)		IF TAB_r(r)	+ C@ THEN
 #%r_x,(%r_x,%r_x)	IF TAB_r(r,r)	+ C@ THEN
 REX_W OR $40 OR C,
  0 TO REX_W  0 TO REX.RBX
;

: ?REX,  REX IF REX, THEN  ;

: DO|;  ( cod -- )
  ?REX,
  OR  C,
 0 TO PARM_HESH 0 TO REG>8 ;

: |;
 POSTPONE  DO|;
 POSTPONE ;
; IMMEDIATE

: ret,		$c3 C, ;
: retq,		$c3 C, ;
: cwtl,		$98 C, ;
: cltd,		$99 C, ;
: fwait, 	$9b C, ;
: pushfq,	$9c C, ;
: popfq, 	$9d C, ;
: sahf,  	$9e C, ;
: lahf,  	$9f C, ;
: lret,		$cb C, ;
: int3,		$cc C, ;
: icebp,  	$f1 C, ;
: hlt,		$f4 C, ;
: cmc,		$f5 C, ;

: clc,		$f8 C, ;
: stc,		$f9 C, ;
: cli,		$fa C, ;
: sti,		$fb C, ;
: cld,		$fc C, ;
: std,		$fd C, ;

: REG? 0= IF -333 THROW THEN ;
: POP,    #%r_x REG? $50 |;
: PUSH,   #%r_x REG? $58 |;

: not_  ( c -- cod )
  ?REX,  $f6 REG>8 OR C, ;


: not,	not_ $D0 |;
: neg,	not_ $D8 |;
: mul,	not_ $E0 |;
: imul,	not_ $E8 |;
: div,	not_ $F0 |;
: idiv,	not_ $F8 |;

: inc, 	?REX, $ff C, $C0 |;
: dec, 	?REX, $ff C, $C8 |;


CREATE <<BUF
  0 C, 1 C, 1 C, 2 C,
  2 C, 2 C, 2 C, 3 C,

: (()),
 #(%r_x)
 IF	OVER DUP 4 = SWAP 5 = OR ((OFFSET OR
  IF	((OFFSET SHORT?
	IF	$40 OR OVER DO|; 4 = IF $24 C, THEN ((OFFSET C,
	BREAK	$80 OR OVER DO|; 4 = IF $24 C, THEN ((OFFSET L,
  BREAK
     OVER DO|; 4 = IF $24 C, THEN
 BREAK
 #(%r_x,%r_x) 
 IF	((OFFSET
  IF	((OFFSET SHORT? \ ." #(%r_x,%r_x)=<" CR .S ." >" CR KEY DROP
	IF	$44 DO|; 1- <<BUF + C@ 3 << OR  3 << OR C, ((OFFSET C, 
	BREAK	$84 DO|; 1- <<BUF + C@ 3 << OR  3 << OR C, ((OFFSET L, 
  BREAK
     4 DO|;  1- <<BUF + C@ 3 << OR  3 << OR C,
 BREAK
   -333 THROW
  ;

: $f7(()), ?REX, $f7 C, (()), ;
: notl,		$10 $f7(()), ;
: negl,		$18 $f7(()), ;
: mull,		$20 $f7(()), ;
: imull,	$28 $f7(()), ;
: divl,		$30 $f7(()), ;
: idivl,	$38 $f7(()), ;

: notq,		$8 TO REX_W notl,	; 
: negq,		$8 TO REX_W negl,	; 
: mulq,		$8 TO REX_W mull,	; 
: imulq,	$8 TO REX_W imull,	; 
: divq,		$8 TO REX_W divl,	; 
: idivq,	$8 TO REX_W idivl,	;  

: >PARM ( cfa )  >BODY @ TO PARM_HESH ;

: ADD| ( c -- cod )
  ?REX,
  #$,%r_x    IF DUP	$88 =
		IF	$C6 REG>8 OR C, $48
		ELSE	$80 REG>8 OR C, $C0
		THEN  XOR  REG>8 IF DO|; L, BREAK DO|; C, BREAK
\  #%_l,%_l   IF C, SWAP 3 << OR $C0 DO|; BREAK

 REG>8 OR

  #%r_x,%r_x		IF C, SWAP 3 << OR $C0 DO|; BREAK

  #%r_x,(%r_x)		IF C, SWAP 3 << ['] #(%r_x)	>PARM (()), BREAK
  #%r_x,(%r_x,%r_x)	IF C, ROT  3 << ['] #(%r_x,%r_x) >PARM (()), BREAK

  2 OR

  #(%r_x),%r_x		IF C, 3 << ['] #(%r_x)		>PARM (()), BREAK
  #(%r_x,%r_x),%r_x	IF C, 3 << ['] #(%r_x,%r_x)	>PARM (()), BREAK
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
: mov,	0x88 ADD| ;

: movzb, ?REX, $F C, 0xB6 ADD| ;

: xchg,

  #%r_x,%r_x		IF 0x86 ADD|  BREAK

  #(%r_x),%r_x		IF 0x84 ADD|  BREAK
  #(%r_x,%r_x),%r_x	IF 0x84 ADD|  BREAK 

  #%r_x,(%r_x)		IF SWAP ['] #(%r_x),%r_x	>PARM 0x84 ADD| BREAK
  #%r_x,(%r_x,%r_x)	IF  ROT ['] #(%r_x,%r_x),%r_x	>PARM 0x84 ADD| BREAK 

  -333 THROW
 ;

: test,

  #%r_x,%r_x		IF 0x84 ADD|  BREAK

  #%r_x,(%r_x)		IF 0x84 ADD|  BREAK
  #%r_x,(%r_x,%r_x)	IF 0x84 ADD|  BREAK 

  #(%r_x),%r_x		IF SWAP [']  #%r_x,(%r_x)	>PARM 0x84 ADD|  BREAK
  #(%r_x,%r_x),%r_x	IF -ROT [']  #%r_x,(%r_x,%r_x)	>PARM 0x84 ADD|  BREAK 

  -333 THROW
 ;


: lea,

  #(%r_x),%r_x		IF SWAP [']  #%r_x,(%r_x)	>PARM 0x8C ADD|  BREAK
  #(%r_x,%r_x),%r_x	IF -ROT [']  #%r_x,(%r_x,%r_x)	>PARM 0x8C ADD|  BREAK 

  -333 THROW
 ;


\  DO|;

: #lp, C, 1- HERE - C, ;

: loopne,	$e0 #lp, ;
: loope,	$e1 #lp, ;
: loop,		$e2 #lp, ;
: jrcxz,	$e3 #lp, ;

: j?   OVER 1- HERE - SHORT? IF $70 OR #lp, BREAK
		$f C,  $80 + C, 4 - HERE - L,  ;
: jo,  	$0 j? ;
: jno, 	$1 j? ;
: jb,  	$2 j? ;
: jae, 	$3 j? ;
: je,  	$4 j? ;
: jne, 	$5 j? ;
: jbe, 	$6 j? ;
: ja,  	$7 j? ;
: js,  	$8 j? ;
: jns, 	$9 j? ;
: jp,  	$a j? ;
: jnp, 	$b j? ;
: jl,  	$c j? ;
: jge, 	$d j? ;
: jle, 	$e j? ;
: jg,  	$f j? ;





VARIABLE NLASTS NLASTS 0!
0 VALUE TEB_CLEAN

: CODL
[IFDEF] TEXEC_BUF
	>IN M@ >R
	PARSE-NAME
\ 	ALSO FORTH
 SFIND
\ PREVIOUS

	IF \	NPASS 0=
\		IF
			TEB_CLEAN 4 + TO TEB_CLEAN
			TEXEC_BUF TEXEC_BUF CELL+ TEB_SIZE MOVE
			TEXEC_KEY TEXEC_KEY CELL+ TEB_SIZE MOVE
\		THEN
			TEXEC_BUF M!
		HERE	TEXEC_KEY M!
	ELSE	2DROP
	THEN

	R> >IN M!
[THEN]
	HEADER

        NLASTS 1+!
;

VECT A?HEAD

' NOOP TO A?HEAD


VARIABLE HERE-TAB-CUR-SAVE

VARIABLE SHERE-TAB-CUR-SAVE

0 VALUE MOREPASSFLG
0 VALUE SAVELAST

: ASM_END

 MOREPASS TO MOREPASSFLG
 MOREPASS IF
\ CR ." MOREPASS"
\+ HERE-TAB-CUR	HERE-TAB-CUR-SAVE @ HERE-TAB-CUR !
\+ SHERE-TAB-CUR	SHERE-TAB-CUR-SAVE @ SHERE-TAB-CUR !
  ITERNUM 2+ TO   ITERNUM
   NPASS 1+ TO NPASS
	0 TO TEB_CLEAN
  0 TO MOREPASS
  SAVELAST CURRENT @ !
   POSTPONE [AGAIN]
   EXIT
 THEN
	PREVIOUS

( !!!!
	LAST M@ NLASTS M@
	BEGIN  DUP
	WHILE A?HEAD LAST M@ CDR LAST M! 1-
	REPEAT NLASTS M!
	LAST M! )
;

0 [IF]
[THEN]
EXPORT

: ASM64_BIG	ALSO ASM64_MOD ;

: (CODE)
 ASM64_BIG
  0 TO MOREPASS
  0 TO NPASS
  0 TO TEB_CLEAN
  CURRENT @ @ TO SAVELAST

  ITERNUM 2+ TO   ITERNUM

\+ HERE-TAB-CUR		HERE-TAB-CUR @ HERE-TAB-CUR-SAVE !
\+ SHERE-TAB-CUR	SHERE-TAB-CUR @ SHERE-TAB-CUR-SAVE !
 POSTPONE [BEGIN]
;

: CODE
 0 TO PARM_HESH
    CODL
   (CODE)  ;


;MODULE
