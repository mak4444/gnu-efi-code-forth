
\ VECT <SET-EXC-HANDLER> \ установить обработчик аппаратных исключений

T: HERE DP @ DUP TO :-SET ;

T: DUP>R
 OPT_INIT
  SetOP  $50 C, OPT \ Push    %rax
 OPT_CLOSE
; IMMEDIATE

T: >R 
 OPT_INIT
 SetOP $50 C,  OPT \ Push    %rax
 OPT_CLOSE
 ['] DROP INLINE,
; IMMEDIATE

T: R>
 ['] DUP INLINE,
 OPT_INIT
 SetOP  $58 C,  OPT \ POP    %rax
 OPT_CLOSE
; IMMEDIATE

T: R@
 ['] DUP INLINE,
 OPT_INIT
 SetOP  $24048B48 L,  OPT \  mov    (%rsp),%rax
 OPT_CLOSE
; IMMEDIATE

T: RDROP
 OPT_INIT
 SetOP $24648D48 L, $08 C,  OPT  \  	lea    0x8(%rsp),%rsp
 OPT_CLOSE
; IMMEDIATE


0 VALUE TO_MOVK
\ : SSSS   TO_MOVK ;

: LIT,0 ( n -- )
\ compile code for a literal
  ['] DUP  INLINE,
  OPT_INIT SetOP

   DUP  $100000000 U< NEWLIT AND
   IF	$B8 C, L,  \   mov $X,%eax
   	OPT   OPT_CLOSE
   BREAK

   DUP  0x80000000 + 0x100000000 U< 
   IF     $48 C,  $c0c7 W,  L,
   ELSE   $48 C,  $B8 C,  , \ OPT  \ MOVABS RAX, #
   THEN
   OPT
   OPT_CLOSE
;

' LIT,0 ->DEFER LIT,

: ADDR,  ( addr -- )
  'DUP INLINE,
   OPT_INIT     SetOP
	$48 C,  $058d  W,  THERE 4 + - L,  \   mov    N(%rip),%rax
      OPT
  OPT_CLOSE
;

: ?VALUE ( cfa -- cfa flg)  DUP 1+ DUP SL@ + 4 + ['] DOVALUE = ;
: ?CONST ( cfa -- cfa flg)  DUP 1+ DUP SL@ + 4 + ['] DOCONSTANT = ;
: ?VAR   ( cfa -- cfa flg)  DUP 1+ DUP SL@ + 4 + ['] DOCREATE = ;

T: _COMPILE, ( cfa -- )
  0xE8 C, \ CALL
  HERE 4 + - L,
;

T: COMPILE,
  ?CONST IF EXECUTE LIT, BREAK
  ?VAR	 IF EXECUTE ADDR, BREAK
  ?VALUE IF >BODY ADDR, 'GET INLINE, BREAK
	INLINE? IF  INLINE, BREAK
	_COMPILE, ;

: NUMBER,      ( d -- )
                DOUBLE? 0= IF DROP THEN
                STATE @
                IF      DOUBLE? IF  SWAP  LIT,  THEN
                        LIT,
                THEN
;

: XXX-SLITERAL ( addr u -> d true | false ) 
   NUMBER?
 IF NUMBER, TRUE  EXIT
 THEN
   2DROP FALSE
;

: BIN-SLITERAL ( addr u -> d true | false )
  BASE @ >R 2 BASE !
  XXX-SLITERAL
  R> BASE !
;

: HHH-SLITERAL ( addr u -> d true | false )
  BASE @ >R HEX
  2- SWAP 2+ SWAP
  XXX-SLITERAL
  R> BASE !
;


: UPC  ( c -- c' )
   DUP  [CHAR] a [CHAR] z 1+ WITHIN
   IF  0x20 ANDC
   THEN   ;


: SNUMBER ( addr len -- d1 )
 NUMBER? THROW ;

: NUMBER ( a1 -- d1 )
\ Convert count delimited string at a1 into a double number.

\  0 0 ROT COUNT >NUMBER THROW DROP

 COUNT
 NUMBER? 0=
 THROW ;


: ?SLITERAL3_H  ( c-addr u -- ... )
  DUP 1 >
     IF

	 2DUP 2>R
         OVER C@ [CHAR] - = DUP  &?MINUS !
         IF    1 /STRING 
         THEN

         OVER W@ 0x7830 ( 0x) = 
         IF     HHH-SLITERAL
		IF RDROP RDROP
		ELSE -13 THROW
		THEN EXIT
         THEN

          OVER C@ [CHAR] $ = 
         IF 1+ SWAP 1- SWAP HHH-SLITERAL
		IF RDROP RDROP
		ELSE -13 THROW
		THEN EXIT
         THEN
              2DUP + 1- C@ UPC [CHAR] H =
         IF  1+  SWAP 2- SWAP  HHH-SLITERAL
		IF RDROP RDROP
		ELSE -13 THROW
		THEN EXIT
         THEN

             2DUP + 1- C@ UPC [CHAR] B = BASE @ 0x10 <> AND
         IF   1- BIN-SLITERAL
		IF RDROP RDROP 
		ELSE -13 THROW
		THEN EXIT
         THEN

             OVER @ 0xFF00FF  AND 0x270027 ( '\0')  = 
             OVER 3 = AND
         IF  DROP @ 8 RSHIFT 0xFF AND
            STATE @ IF LIT, THEN RDROP RDROP  EXIT
         THEN 
         2DROP 2R>
  THEN
  2DUP 2>R 
 XXX-SLITERAL 0=
  IF  2R>
 2DROP -1
       IF  -13 THROW \ ABORT"  -???"
       ELSE  THROW THEN
  ELSE RDROP RDROP
  THEN
;

' ?SLITERAL3_H ->DEFER ?SLITERAL


\ VARIABLE  TBASE
T: DECIMAL DECIMAL ;

: 2LITERAL_ STATE @ IF SWAP LIT, LIT, THEN ;
:  LITERAL_ STATE @ IF LIT, THEN ;


\ S" 2345" NUMBER? H. H. H.