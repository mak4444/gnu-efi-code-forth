
 : PPC ; 
\ REQUIRE COMMENT:
 ~mak/lib/fpc.f 

 REQUIRE forth ~mak/lib/caseins.f
\+ CASE-INS  CASE-INS ON
\- IMAGE-BEGIN ' IMGLIT VALUE IMAGE-BEGIN

REQUIRE [AGAIN] ~mak/lib/twopass.f 

CREATE SEG_DT
  0 , 0 ,
 -1 ,  0 , -1 ,  0 , -1 ,  0 , -1 ,  0 , -1 , 0 , -1 ,  0 , -1 ,
 -1 ,  0 , -1 ,  0 , -1 ,  0 , -1 ,  0 , -1 , 0 , -1 ,  0 , -1 ,
HERE
  0 , -1 ,
CONSTANT SEG_DT_END


: T> ( tadr -- madr )
 SEG_DT
 BEGIN  CELL+ CELL+  2DUP @ U<
 UNTIL 
 CELL- @ +
;
\  SEG_DT 88 dump
\  here  SEG_DT  CELL+ CELL+  2DUP @  h. h. h. h.

: >SEG_DT ( madr tadr -- )
 SEG_DT
 BEGIN  CELL+ CELL+  2DUP @ U<
 UNTIL  \  madr tadr dtadr
 ." SEG_DT30=" .s cr
 DUP
 DUP CELL+ CELL+ SEG_DT_END OVER - CMOVE>
 >R
 TUCK - SWAP
 R>
 ." SEG_DT60=" .s cr
 2!
 ." SEG_DT70=" .s cr

;

0 VALUE LOW_LIM

VECT PF> 
VECT >PF
WARNING OFF
Meta_x86_64/mhead0.f

\ ~mak/lib/THERE/bsthere.f

REQUIRE TC_?LIMIT ~mak/lib/THERE/STAT.F


src/INIT/srcxx.f 
\- 4+ : 4+ 4 + ;

Meta_x86_64/mhead.f


\ - L, : L, , ;
\ - L! : L! ! ;
\ - L@ : L@ @ ;

 CREATE RMARGIN $30 ,

 MEM_MODE

[IFNDEF] W>S
 : W>S ( w -- n )  \ 
  0xFFFF AND    \ 
 0x8000 XOR 0x8000 - ;
[THEN]

[IFNDEF] H.R
: H.R           ( n1 n2 -- )    \ display n1 as a hex number right
                                \ justified in a field of n2 characters
                BASE @ >R HEX >R
                0 <# #S #> R> OVER - 0 MAX SPACES TYPE
                R> BASE ! ;
[THEN]

: HH.+  DUP C@ 2 H.R ."  " 1+ ;
: INST.+
 HH.+ ;

VECT MINST
: ?.NAME>S      ( CFA -- )
	DUP 0xFFFF0000 AND 0x10000000 <> IF DROP BREAK
	NEAR_NFA 
	>R DUP
	IF ."  ( " DUP COUNT TYPE 
	     NAME> R> - DUP
	     IF   DUP ." +" NEGATE H. \ >S
	     THEN DROP        ."  ) "
	ELSE RDROP DROP
	THEN ;

REQUIRE 'NOOP	Meta_x86_64/SRC/forward.f
\ REQUIRE DISARM		Meta_x86_64/SRC/disgasm64.4
 REQUIRE DISARM		Meta_x86_64/SRC/disgasm.4
' SMINST TO MINST

REQUIRE INCLUDED_AL	Meta_x86_64/SRC/mlist.f 

REQUIRE GASM64_MOD Meta_x86_64/SRC/gasm64.4


\ S" Meta_x86_64\SRC\aarchext.4" INCLUDED

: DROP,
	$086D8D4800458B48 ,
\	$48 C, $8b C, $45 C, $00 C,	\ mov    0x0(%rbp),%rax
\	$48 C, $8d C, $6d C, $08 C,	\ lea    0x8(%rbp),%rbp
 ;
: DUP,
	$00458948F86D8D48 ,
\	$48 C, $8d C, $6d C, $f8 C,	\ lea    -0x8(%rbp),%rbp
\	$48 C, $89 C, $45 C, $00 C,	\ mov    %rax,0x0(%rbp)
 ;

: M\ ( POSTPONE \) ; IMMEDIATE

128         CONSTANT    T-ITABLE-SIZE               \ 

REQUIRE T-START	Meta_x86_64/SRC/tc.f 

\ : MEHO HERE S" MEHO_" EVALUATE DP ! ;
: OSSS HERE S" STARTOS" EVALUATE DP ! ;

: |  2SWAP  SWAP C, C,  SWAP C, C,  ;

\  COM_INIT
\ E> BREAK

0 VALUE ROM-HERE


\ CASE-INS ON

\ T-ROM-SIZE

