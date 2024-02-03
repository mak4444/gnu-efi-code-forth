
REQUIRE GASM64_MOD ~mak\ARM7\SRC\gasm64.4th

REQUIRE NUMBER? ~mak\LIB\fpcnum.f 

[IFNDEF]   SCAN
: SCAN ( adr len char -- adr' len' )
\ Scan for char through addr for len, returning addr' and len' of char.
        >R 2DUP R> -ROT
        OVER + SWAP
        ?DO DUP I C@ =
                IF LEAVE
                ELSE >R 1 -1 D+ R>
                THEN
        LOOP DROP ;
[THEN]

[IFNDEF] MTOKEN

: PASS\N
  BEGIN  SkipDelimiters  EndOfChunk
  WHILE REFILL 0= IF TRUE  EXIT THEN
  REPEAT      FALSE ;

: MTOKEN ( TABL -- ADDR N )
  PASS\N
  IF DROP CharAddr  0  EXIT THEN
  DUP >R COUNT PeekChar SCAN NIP
  IF RDROP CharAddr 1 DUP >IN +! EXIT THEN
  CharAddr
  BEGIN 1 >IN +!
     EndOfChunk
     IF  TRUE
     ELSE   R@ COUNT PeekChar SCAN NIP
     THEN
  UNTIL   CharAddr OVER -
  RDROP
;
[THEN]


MODULE: GASM64_MOD

VECT LEX_EXECUTE
' EXECUTE TO LEX_EXECUTE

0 VALUE ?LEX
0 VALUE ?,

CREATE ASM64_TABL HERE
  S"  ,#@()$"  S",
  0x9 C, CHAR " C,
HERE OVER - 1- SWAP C!

[IFNDEF]  M@ :  M@  @ ; [THEN]
[IFNDEF] MC@ : MC@ C@ ; [THEN]

0 VALUE ASM_DEPTH
0 VALUE ,_DEPTH

: ASM_INTERPRET ( -> )
\ CR ." ASM_INTERPRET=" >IN @ 0 PARSE TYPE >IN !
  TRUE  TO ?LEX
 DEPTH  TO ASM_DEPTH
 DEPTH  TO ,_DEPTH

  BEGIN \ PARSE-NAME
      SkipDelimiters  EndOfChunk
     IF 0 0 
     ELSE ASM64_TABL MTOKEN \ ." |"  >IN @ 0 PARSE TYPE >IN ! ." <" 2DUP TYPE  ." >" 
     THEN  DUP
  WHILE
	SFIND  ?DUP  
    IF
         STATE M@ =
        IF COMPILE, ELSE LEX_EXECUTE THEN
    ELSE

\         S" NOTFOUND" SFIND 
\         IF EXECUTE
\         ELSE 2DROP
 ?SLITERAL
\ THEN
    THEN
    ?STACK

  REPEAT
\ ." AEE=" H. H. \
 2DROP
  FALSE  TO ?LEX
 0 TO ?, ;

: ;  ?LEX 0=  IF	POSTPONE ;	BREAK POSTPONE	\ ; IMMEDIATE
: $  $$ ;
: ,  ?LEX 0=  IF		,	THEN -1 TO ?, DEPTH  TO ,_DEPTH ;
: @  ?LEX 0=  IF		@ 	THEN POSTPONE	\ ;
: (  ?LEX 0=  IF	POSTPONE ( 	BREAK
   DEPTH  ?, IF ,_DEPTH ELSE ASM_DEPTH THEN - 0= IF 0 THEN  ((  ;

: )  )) ;
: //  POSTPONE	\ ; IMMEDIATE
0 VALUE V.DEPTH
: .byte|  DEPTH  V.DEPTH > IF >R RECURSE R> C, THEN ;

 : .byte DEPTH TO V.DEPTH ASM_INTERPRET .byte| ;
\ : ASM_END  ASM_END ;

: END-CODE ASM_END ;

EXPORT

: ASM_INTERPRET ASM_INTERPRET ;
;MODULE
