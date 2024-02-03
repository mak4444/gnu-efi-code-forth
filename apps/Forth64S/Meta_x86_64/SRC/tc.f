

[IFNDEF] OR!	:  OR! ( N ADDR -- )	DUP @ ROT OR	SWAP ! ; [THEN]
[IFNDEF] AND!	: AND! ( N ADDR -- )	DUP @ ROT AND	SWAP ! ; [THEN]

: L+!  ( N ADDR -- )	DUP L@ ROT + SWAP L! ;
\- BOUNDS : BOUNDS OVER + SWAP ;
\- ?THROW : ?THROW SWAP IF THROW THEN DROP ;
\- ?PAIRS : ?PAIRS	( x1 x1 -- )  XOR -22 ?THROW ;

0 VALUE MSTR_IMG

REQUIRE NUMBER? ~mak/lib/fpcnum.f 
REQUIRE 'NOOP	Meta_x86_64/SRC/forward.f

: CMOVE ( c-addr1 c-addr2 u --- )
\ Copy u bytes starting at c-addr1 to c-addr2, proceeding in ascending
\ order.

   DUP IF  >R
   BEGIN
    OVER 
\ ." C=" DUP H.
 C@
 SWAP DUP >R C!
 R> 1+ SWAP 1+ SWAP
    R>  1- DUP  >R 0=
   UNTIL
   R>
   THEN
   2DROP DROP
;

: cmove>	\ addr1 addr2 len --
  dup if
    tuck over + 1-			\ -- addr1 len addr2 addr2+len-1
    2swap + -rot			\ -- addr1+len addr2 addr2+len-1
    do  1- dup c@ i  c!  -1 +loop
    drop  exit
  endif
  2drop drop
;
[then]

: move		\ addr1 addr2 u --
  -rot 2dup u<
  if  rot cmove>  else  rot cmove  THEN
;

: M_CMOVE ( c-addr1 c-addr2 u --- )
\ Copy u bytes starting at c-addr1 to c-addr2, proceeding in ascending
\ order.

   DUP IF  >R
   BEGIN
    OVER 
 MC@
 SWAP DUP >R
 C!
 R> 1+ SWAP 1+ SWAP
    R>  1- DUP  >R 0=
   UNTIL
   R>
   THEN
   2DROP DROP
;

[IFNDEF] #HEADER
: #HEADER ( cfa -- )
  HERE >R
  DP M! HEADER
 R> DP M! ;
[THEN]

[IFNDEF] ALIGN-NOP
: ALIGN-NOP ( n -- )
\ выровнять HERE на n и заполнить NOP
  HERE DUP ROT 2DUP
  MOD DUP IF - + ELSE 2DROP THEN
  OVER - DUP ALLOT 0x90 FILL
;
[THEN]

: XOR!	DUP @ ROT XOR SWAP ! ;

\+ OPTVALNEED 0 TO OPTVALNEED
 Meta_x86_64/SRC/macroopt.4
\ REQUIRE INLINE? src/macroopt.f 
 ~mak/OptTr.f 
 ~mak/DOP.F 

: DTSTON   ['] DoDTST TO DTST ; IMMEDIATE
: DTSTOFF   ['] DROP TO DTST ; IMMEDIATE
: MINLINE? INLINE? ;

: IHERE HERE DUP TO :-SET ;

0 VALUE ?SOURCETYPE
: SOURCETYPE_SET -1 TO ?SOURCETYPE ;

MODULE: TC


: ><DP DP M@ T-DP M@
       DP M! T-DP M! ;

: DHERE	D-DP M@ ;
: DP><DDP    DHERE IHERE  D-DP M! DP M! ;

: I, , ;

: T_COMPILE,
  0xE8 C, \ CALL
  IHERE 4 + - L,
 ;

: SUBST ( cfa adr -- )
  IHERE >R DP M!
  T_COMPILE,
  R>  DP M!
  
;

-1 VALUE RRRPP
0 VALUE 'PEXIT
\ -1 VALUE SPEED?

: IALLOT ALLOT ;
0 VALUE TO_MOVK

: TI_COMPILE,
	INLINE? IF  INLINE, BREAK
	T_COMPILE, ;

-1 VALUE NEWLIT

: NEWLITON  -1 TO NEWLIT ; IMMEDIATE
: NEWLITOFF  0 TO NEWLIT ; IMMEDIATE

: TC-LIT, ( n -- )
  'DUP INLINE,
   OPT_INIT     SetOP
   DUP  'FSTART - $FFFFFF U<
   IF	  $48 C,  $058d  W,  HERE 4 + - L,  \   mov    N(%rip),%rax
      OPT  OPT_CLOSE
   BREAK
   DUP  $100000000 U< NEWLIT AND
   IF	$B8 C, L,	\ mov $X,%eax
   	OPT   OPT_CLOSE
   BREAK

   DUP  $80000000 + $100000000 U< 
   IF     $48 C,  $c0c7 W,  L,
   ELSE   $48 C,  $B8 C,  , \ OPT  \ MOVABS $X,%rax  
   THEN
   OPT   OPT_CLOSE

;

: T>BODY 5 + ;

: T?VALUE ( cfa -- cfa flg)  DUP 1+ DUP L@ L>S + 4 + 'DOVALUE = ;
: TCOMPILE, ( tcfa -- )  
  DUP THERE? 0=
  IF	?CONST IF EXECUTE TC-LIT, BREAK
\+	?VALUE ?VALUE  IF EXECUTE TC-LIT, BREAK
\   T?VARIABLE IF EXECUTE TC-LIT, BREAK
      -9 THROW
  THEN
   T?VALUE IF T>BODY TC-LIT, 'GET INLINE, BREAK
  TI_COMPILE,
 ;

: DO_COMPILE, ( tcfa -- )  
 SHERE-TO-TAB T_COMPILE, SHERE-TO-TAB ;

: TCONSTANT ( n -- )	CON:THS ><DP CONSTANT   ><DP ;

: CONSTANT HERE THERE? IF TCONSTANT BREAK CONSTANT ;

: TENUM ( n -- )	CON:THS ><DP ENUM  ><DP ;

: ENUM HERE THERE? IF TENUM BREAK ENUM ;


0 VALUE ZZZZ

: COMPILE,  
\ ZZZZ IF  THEN
 HERE
 THERE?
 IF TCOMPILE,
 BREAK
 COMPILE, ;


: [COMPILE] '  COMPILE,  ; IMMEDIATE

0 VALUE 'DO-CONST

: ICONSTANT
\ abort
 HEADER
    'DO-CONST ,                                                    \ 
    ,                                                             \ 
 ;

: #define  HERE THERE? IF ><DP #define ><DP BREAK #define ;

: DLIT, ( D -> )
  SWAP LIT, LIT,
;

: TLIT,
 HERE THERE?
 IF  TC-LIT, BREAK
 [ ' LIT, DEFER@ COMPILE, ] ;


' TLIT,  TO  LIT,

: MCREATE CREATE  ;

: LABEL        ( | name --- )  \ 
   IHERE    TCONSTANT ;

: CREATE
   IHERE  \ $7 + $7 ANDC DUP DP M!
  TCONSTANT ;

: NEXT
	$C3 C, \  RET
 ;

\ : ALLOT  DP M+! ;

: MVARIABLE VARIABLE ;
: VARIABLE    ALIGN CREATE 1 CELLS  ALLOT ;
: 2VARIABLE   ALIGN CREATE 2 CELLS  ALLOT ;

: ?OLD
  S"  HERE THERE? 0= IF" EVALUATE
  POSTPONE POSTPONE
  S" EXIT THEN" EVALUATE ; IMMEDIATE

0 VALUE 'EXIT

: EXIT, $C3 C, ;

: EXIT
	?OLD EXIT	EXIT,
; IMMEDIATE

: T_RECURSE   \ 94
\ Итерпретация: семантика не определена.
\ Компиляция: ( -- )
\ Добавить семантику выполнения текущего определения в текущее определение.
\ Неоднозначная ситуация возникает, если RECURSE используется после DOES>.
\	?OLD RECURSE
  LAST @ NAME>  T_COMPILE,
; IMMEDIATE


 0 VALUE ;OPT?
 0 value DOES_FLG
 
: ; 
	?OLD ;
		EXIT,
	SMUDGE	[COMPILE] [
	SHERE-TO-TAB
; IMMEDIATE


: BRANCH, ( ADDR -> )  \ скомпилировать инструкцию ADDR JMP
   0xE9 C,
  DUP IF IHERE 4 + - THEN L,  \  DP @ TO LAST-HERE
;


: ?BRANCH, ( A -- )
 0x84 TO J_COD
   OPT_INIT SetOP $48 C, $C085 W,  OPT \ test   %rax,%rax
   OPT_CLOSE  	'DROP INLINE,
\   SetOP
     0x0F C,  J_COD C,
  DUP IF IHERE 4 + - THEN L, \ DP @ TO LAST-HERE
 0 TO J_COD

;

1	CONSTANT IF_FLAG
13	CONSTANT HEAD_FLAG
7	CONSTANT BEGIN_FLAG	
3	CONSTANT DO_FLAG1

: T>ORESOLVE1 ( A -> )
    IHERE
    OVER - 4 -
    SWAP     L!
;

: T>ORESOLVE ( A, N -- )
  DUP IF_FLAG = IF   DROP T>ORESOLVE1
          ELSE HEAD_FLAG <> IF -2007 THROW THEN \ ABORT" Conditionals not paired"
               T>ORESOLVE1
          THEN
;

\ : FORWARD_RESOLVE  ( adr_after_?BRANCH_BRANCH target_adr -- )
\    SWAP L! ;

\- ?COMP : ?COMP   ?STATE 0= IF -312 THROW THEN ; ( Только для режима компиляции )

: AHEAD
\+ AHEAD ?OLD AHEAD
	?COMP THERE  BRANCH,	HERE 4 - HEAD_FLAG	; IMMEDIATE

: IF	?OLD IF	
	?COMP THERE ?BRANCH,	HERE 4 - IF_FLAG	; IMMEDIATE
: THEN	?OLD THEN	?COMP 
  T>ORESOLVE ; IMMEDIATE
: BREAK ?OLD BREAK	POSTPONE EXIT POSTPONE THEN ; IMMEDIATE

: TCS-SWAP 2SWAP ;
: TCS-DUP  2DUP ;

: ELSE   ( BO BI ADDR ? -- 0 0 ADDR1 ?1 )
  HERE THERE? 0= IF POSTPONE ELSE BREAK
  POSTPONE  AHEAD TCS-SWAP
  POSTPONE  THEN
;  IMMEDIATE

S" Meta_x86_64/SRC/case.4" INCLUDED

: BEGIN
  HERE THERE? 0= IF POSTPONE BEGIN EXIT THEN
  ?COMP IHERE
\  CELL-
 3
 ; IMMEDIATE

: UNTIL \ 94
  HERE THERE? 0= IF POSTPONE UNTIL EXIT THEN
  3 <> IF -2004 THROW THEN \ ABORT" UNTIL 
  ?COMP
  ?BRANCH,

; IMMEDIATE

: WHILE \ 94
  HERE THERE? 0= IF POSTPONE WHILE EXIT THEN
  ?COMP [COMPILE] IF
  TCS-SWAP
; IMMEDIATE

: AGAIN  HERE THERE? 0= IF  POSTPONE AGAIN EXIT THEN
  ?COMP
  ?COMP 3 <> IF -2006 THROW THEN \ ABORT" AGAIN 
  BRANCH,
; IMMEDIATE

: REPEAT HERE THERE? 0= IF  POSTPONE REPEAT EXIT THEN
  POSTPONE AGAIN
  POSTPONE THEN 
; IMMEDIATE

: M_WL TCS-DUP POSTPONE WHILE ; IMMEDIATE

: RECURSE 
  ?COMP
\	
  LAST-NON
  DUP 0= 
  IF
 DROP 
 LATEST NAME>
  THEN
 HERE  THERE?
 IF
  T_COMPILE,
 BREAK
  COMPILE,
; IMMEDIATE

: [']  \ 94
  ?COMP
	?OLD [']
  ' 
 LIT, 
; IMMEDIATE

: [CHAR]
  ?COMP
  PARSE-NAME  DROP MC@  LIT,
; IMMEDIATE

: [2CHAR]
  ?COMP
  PARSE-NAME  DROP MW@  LIT,
; IMMEDIATE

: S", ( addr u -- ) \ 
  DUP  C, 
 IHERE SWAP DUP IALLOT
\  TT_? IF  THEN
   M_CMOVE ;

: ALIGNED  1+ -2 AND ;


: SLITERAL
\  HERE THERE? DROP
  IHERE THERE?  STATE M@ AND
  IF '(S")  T_COMPILE, S",
  ELSE  POSTPONE SLITERAL
  THEN   ; IMMEDIATE

: S"  ( "ccc<quote>" --- )
\ Parse a string delimited by " and compile the following runtime semantics.
\ Runtime: ( --- c-addr u) Return start address and length of that string.
  [CHAR] " PARSE
  POSTPONE SLITERAL
 ; IMMEDIATE

: Z"  ( "ccc<quote>" --- )
\ Parse a string delimited by " and compile the following runtime semantics.
\ Runtime: ( --- c-addr u) Return start address and length of that string.
  [CHAR] " PARSE
   '(Z")
\  IHERE - 2 >> $3FFFFFF AND  $94000000 OR  L,  
  T_COMPILE,
 1+ S", -1 IALLOT 0 C,
\	HERE 1 AND IF 0xFF C, THEN
\	HERE 3 AND IF 0xFFFF W,	THEN
 ; IMMEDIATE

0 VALUE '(.")
: ."
	?COMP
	HERE THERE? 0= IF  POSTPONE ." EXIT THEN
	POSTPONE S"
	S" TYPE" EVALUATE
; IMMEDIATE

: ABORT"
	?COMP
	HERE THERE? 0= IF  POSTPONE ABORT" EXIT THEN
	POSTPONE S"
	S" (ABORT'')" EVALUATE
; IMMEDIATE



: 2RDROP
 0x48 C, 0x83 C, 0xC4 C, 0x10 C, \	add    $0x10,%rsp
;   IMMEDIATE

: DO            \ Run: n1|u1 n2|u2 -- ; R: -- loop-sys           6.1.1240
\ *G Begin a *\fo{DO ... LOOP} construct. Takes the end-value and
\ ** start-value from the stack.
  '(DO) TI_COMPILE, HERE 0 , HERE DO_FLAG1
; IMMEDIATE

: ?DO           \ Run: n1|u1 n2|u2 -- ; R: -- | loop-sys ; 6.2.0620
\ *G Compile a *\fo{DO} which will only begin loop execution if
\ ** the loop parameters do not specify an interation count of 0.
  '(?DO) TI_COMPILE, HERE 0 , HERE DO_FLAG1
; IMMEDIATE

: LOOP          \ Run: -- ; R: loop-sys1 -- | loop-sys2         6.1.1800
\ *G The closing statement of a *\fo{DO ... LOOP} construct.
\ ** Increments the index and terminates when the index crosses
\ ** the limit.
  DO_FLAG1 ?PAIRS
 $49 C, $C6FF W, \ inc r14
 $49 C, $C7FF W, \ inc r15
  HERE 2+ -  DUP SHORT? \  SetOP SetJP
  IF
    0x71 C, C, \ jno short 
  ELSE
    4 - 0xF C, 0x81 C, L, \ jno near
  THEN  \ SetOP
  $5E41 W, \ pop r14
  $5F41 W, \ pop r15
  $59 C, \ pop rcx
  HERE SWAP !
 ; IMMEDIATE

: +LOOP         \ Run: n -- ; R: loop-sys1 -- | loop-sys2       6.1.0140
\ *G As *\fo{LOOP} except that you specify the increment on the
\ ** stack. The action of *\fo{n +LOOP} is peculiar when n is
\ ** negative:
\ *C   -1 0 ?DO  i .  -1 +LOOP
\ *P prints *\fo{0 -1}, whereas:
\ *C   0 0 ?DO  i .  -1 +LOOP
\ *P prints nothing. This a result of the mathematical trick used
\ ** to detect the terminating condition. To prevent confusion
\ ** avoid using *\fo{n +LOOP} with negative *\i{n}.
  DO_FLAG1 ?PAIRS
   OPT_INIT
	SetOP $49 C, $c601 W, OPT	\   	add    %rax,%r14
	SetOP $49 C, $c701 W, OPT	\	add    %rax,%r15
   OPT_CLOSE  	'DROP INLINE,
  HERE 2+ -  DUP SHORT? \  SetOP SetJP
  IF
    0x71 C, C, \ jno short 
  ELSE
    4 - 0xF C, 0x81 C, L, \ jno near
  THEN  \ SetOP
  $5E41 W, \ pop r14
  $5F41 W, \ pop r15
  $59 C, \ pop rcx
  HERE SWAP !
 ; IMMEDIATE

: I
  'DUP INLINE,
 OPT_INIT
 SetOP $4c C, $f089 W,  OPT	\	mov %r14,%rax
 OPT_CLOSE
; IMMEDIATE

: ]L  LIT, ] ;

: TO
  ?OLD TO
  '
 STATE @
 IF  DUP THERE? 0=  IF -9 THROW  THEN
   T>BODY TLIT, S" !" EVALUATE 
\ TLIT,   S" DEFER!" EVALUATE 
 BREAK
 DUP THERE?
 IF
	  T>BODY
	  !
 BREAK
  
	  >BODY
	  !

 ; IMMEDIATE


: DUP>R
 	?OLD DUP>R
  OPT_INIT
 SetOP $50 C, OPT \ Push    %rax
 OPT_CLOSE
; IMMEDIATE


: >R 
 	?OLD >R
 OPT_INIT
 SetOP $50 C,  OPT \ Push    %rax
 OPT_CLOSE
 'DROP INLINE,
; IMMEDIATE

: R>
 	?OLD R>
 'DUP INLINE,
 OPT_INIT
 SetOP  $58 C, OPT \ POP    %rax
 OPT_CLOSE
; IMMEDIATE

: R@
 	?OLD R@
 'DUP INLINE,
 OPT_INIT
 SetOP  $24048B48 L, OPT \  mov    (%rsp),%rax
 OPT_CLOSE
; IMMEDIATE

: RDROP
 	?OLD RDROP
 OPT_INIT
 SetOP $24648D48 L, $08 C, OPT \  	lea    0x8(%rsp),%rsp
 OPT_CLOSE
; IMMEDIATE

: R>DROP 	?OLD R>DROP	[COMPILE] RDROP ; IMMEDIATE



[IFDEF] TEXEC_BUF
: EXEC_BUF_SET
	>IN M@ >R
	PARSE-NAME
 SFIND

	IF
			TEXEC_BUF TEXEC_BUF CELL+ TEB_SIZE MOVE
			TEXEC_KEY TEXEC_KEY CELL+ TEB_SIZE MOVE
			TEXEC_BUF M!
		HERE	TEXEC_KEY M!
	ELSE	2DROP
	THEN

	R> >IN M!
;
[ELSE]
: EXEC_BUF_SET ;
[THEN]

: T:
    HERE THERE? 0= IF  :  BREAK
\  HERE 1 AND IF $FF C, THEN
\  HERE 2 AND IF $FFFF W, THEN
   :#THS
  EXEC_BUF_SET
  :
 SHERE-TO-TAB
;

: .qwerty ." qwerty" cr ; immediate

: :_tst
 ." :_tst" cr
    HERE THERE? 0= IF  :  BREAK
 ." :22=:" cr
   :#THS
 ." :29=:" cr
  : 
\ SHERE-TO-TAB
 HERE TO :-SET
 ." :33= ok" cr
;

: :
    HERE THERE? 0= IF  :  BREAK
   :#THS
  : 
 SHERE-TO-TAB
 HERE TO :-SET
;

: IMMEDIATE
  IMMEDIATE
  1 TO TLASTFLG
;

: L:
    HERE THERE? 0= IF  :  BREAK
  : 
  SHERE-TO-TAB
 HERE TO :-SET
;

: THEAD:  :#THS   HEADER ;

: DECIMAL
 STATE @ 0= IF DECIMAL BREAK
 S" $A BASE !" EVALUATE    ; IMMEDIATE

: HEX
 STATE @ 0= IF HEX BREAK
 S" $10 BASE !" EVALUATE    ; IMMEDIATE


: \ [COMPILE] \ ; IMMEDIATE
: ( [COMPILE] ( ; IMMEDIATE
: .( [COMPILE] .( ; IMMEDIATE

0 VALUE VIEW_LINK

0 VALUE TV_CURSTR

: CFA>NLINE ( cfa -- NLine | 0 )
\	NEAR_NFA DROP ?DUP 
\	IF  9 - CELL- M@ 
\	BREAK
 DROP
 TV_CURSTR
;

: VIEW_{
  HERE VIEW_LINK L, TO VIEW_LINK -2 ,

;

: }VIEW_PATH
  HERE VIEW_LINK L, TO VIEW_LINK -1 ,
   PARSE-NAME S",
;


: DOES>  S" (DOES>) R>" EVALUATE
; IMMEDIATE

: VALUE 
   DP M@ THERE? 0= IF  VALUE   BREAK
\ F7_ED
 :#THS
	HEADER
 'DOVALUE DO_COMPILE, I,
;  

0 VALUE 'CRASH

	
: ->DEFER
 :#THS
  EXEC_BUF_SET
	HEADER 
 'DOVECT DO_COMPILE, I,
;  

: DEFER
 ?OLD   VECT
  'CRASH ->DEFER

 ;

: TCONSTANT:
 :#THS
	HEADER
 	'DOCONSTANT DO_COMPILE, I,

;

: FIELD
 :#THS
	HEADER 
 'DOFIELD  DO_COMPILE,  OVER L, + 
;  

: MO_TST_VAL-ON  $FFC7FF TO MO_TST_VAL ; IMMEDIATE
: MO_TST_VAL-OFF $0 TO MO_TST_VAL ; IMMEDIATE

: THERE HERE ;
: HERE HERE DUP TO :-SET ;

EXPORT

1 [IF]

' <PRE> >BODY M@  ' NOOP  TO <PRE> \ 

[DEFINED] DBG_COUNT [IF] DBG_COUNT M@  DBG_COUNT 0! [THEN]

[DEFINED] DBG_COUNT   [IF]  DBG_COUNT ! [THEN]

TO <PRE>

: T-STOP
  PREVIOUS DEFINITIONS
  HERE THERE?    IF ><DP THEN
;

: T-GO
[DEFINED] DBG_STOP [IF]  DBG_STOP [THEN]
  ALSO TC DEFINITIONS
  HERE THERE? 0= IF ><DP THEN
;

\ : THERE?A  IMAGE-BEGIN   U< ;
: THERE?B  'FSTART - $880000  U< ;

: TTH  ['] THERE?B TO THERE? ;

: SEG: ( len seg -- )
  HERE $14 + OVER >SEG_DT
  HERE TCONSTANT , DUP , IALLOT
  0 L,
 ; 

: SEG_SET
 M@ DP M!
;

:  ?COMP_ STATE M@ =
         IF  COMPILE,
         ELSE EXECUTE
         THEN ;

-1 VALUE N.FLOAD?

: TC-INTERPRET ( -> )
\ ?SOURCETYPE IF  ." SOURCE=" SOURCE H. H. CR  KEY DROP THEN
 ?SOURCETYPE IF  CR SOURCE TYPE ( KEY DROP) THEN
\  STATE M@ 0= IF  MAIN_S THEN
  BEGIN
    PARSE-NAME DUP
  WHILE
    SFIND ?DUP
    IF [']  ?COMP_  CATCH 
        THROW
    ELSE 
[IFDEF] ?_.
	?_.  N.FLOAD? AND
        IF  F-SIZE @ >R FSINGLE ['] MFFFFF CATCH R> F-SIZE ! THROW
		STATE M@ IF LIT, THEN
        ELSE ?SLITERAL
        THEN
[ELSE] ?SLITERAL
[THEN]
\         S" NOTFOUND" SFIND 
	\         IF EXECUTE
\         ELSE 2DROP
\         ?SLITERAL
\         THEN
    THEN
    ?STACK
  REPEAT 2DROP 
\  STATE M@ 0= IF  MAIN_S THEN
;

 ' TC-INTERPRET  TO INTERPRET

[THEN]


;MODULE
: [T] ALSO TC ; IMMEDIATE

